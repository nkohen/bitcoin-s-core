package org.bitcoins.dlc.testgen2

import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int32, UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.dlc.{
  DLCFundingInput,
  DLCMessage,
  FundingSignatures
}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{
  Bech32Address,
  BitcoinAddress,
  P2PKHAddress,
  P2SHAddress
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.core.wallet.utxo.{
  ConditionalPath,
  InputInfo,
  P2SHNestedSegwitV0InputInfo,
  P2WPKHV0InputInfo,
  P2WSHV0InputInfo
}
import org.bitcoins.crypto._
import org.bitcoins.dlc.testgen.DLCTestUtil
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.Random

// TODO: Worry about Random.nextDouble being un-bounded, probably introduce var
object DLCTLVGen {

  val defaultAmt: Satoshis = CurrencyUnits.oneBTC.satoshis

  val numDigits: Int = 17

  val doubleBound: Double = (1L << numDigits).toDouble

  def randomDouble(): Double = {
    Random.between(1 - doubleBound, doubleBound)
  }

  def tlvPoint(outcome: Long, value: Double = randomDouble().abs): TLVPoint = {
    val roundedPayout = Satoshis(value.toLong)
    val extraPrecision = (value - roundedPayout.toLong) * (1 << 16)
    TLVPoint(outcome, roundedPayout, extraPrecision.toInt)
  }

  def tlvPoints(numPoints: Int, maxVal: Long): Vector[TLVPoint] = {
    0.until(numPoints - 2)
      .toVector
      .map { _ =>
        tlvPoint(Random.nextLong(maxVal))
      }
      .+:(tlvPoint(0))
      .:+(tlvPoint(maxVal))
      .distinctBy(_.outcome)
      .sortBy(_.outcome)
  }

  def signed16PTLVNumber(
      number: Double = randomDouble()): Signed16PTLVNumber = {
    val sign = number < 0
    val roundedNum = number.abs.toLong
    val extraPrecision = (number.abs - roundedNum) * (1 << 16)

    Signed16PTLVNumber(sign, roundedNum, extraPrecision.toInt)
  }

  val linePiece: PolynomialPayoutCurvePieceTLV = {
    PolynomialPayoutCurvePieceTLV(Vector.empty)
  }

  def quadraticPiece(
      outcome: Long,
      value: Double = randomDouble().abs): PolynomialPayoutCurvePieceTLV = {
    PolynomialPayoutCurvePieceTLV(Vector(tlvPoint(outcome, value)))
  }

  def cubicPiece(
      outcome1: Long,
      outcome2: Long,
      value1: Double = randomDouble().abs,
      value2: Double = randomDouble().abs): PolynomialPayoutCurvePieceTLV = {
    PolynomialPayoutCurvePieceTLV(
      Vector(tlvPoint(outcome1, value1), tlvPoint(outcome2, value2)))
  }

  def inversePiece(
      translatePayout: Double = randomDouble(),
      d: Double = randomDouble()): HyperbolaPayoutCurvePieceTLV = {
    hyperbolaPiece(
      usePositivePiece = true,
      translateOutcome = 0,
      translatePayout = translatePayout,
      a = 1,
      b = 0,
      c = 0,
      d = d
    )
  }

  def hyperbolaPiece(
      usePositivePiece: Boolean = Random.nextBoolean(),
      translateOutcome: Double = randomDouble(),
      translatePayout: Double = randomDouble(),
      a: Double = randomDouble(),
      b: Double = randomDouble(),
      c: Double = randomDouble(),
      d: Double = randomDouble()): HyperbolaPayoutCurvePieceTLV = {
    HyperbolaPayoutCurvePieceTLV(
      usePositivePiece,
      signed16PTLVNumber(translateOutcome),
      signed16PTLVNumber(translatePayout),
      signed16PTLVNumber(a),
      signed16PTLVNumber(b),
      signed16PTLVNumber(c),
      signed16PTLVNumber(d)
    )
  }

  def randomPiece(
      leftEndpoint: TLVPoint,
      rightEndpoint: TLVPoint): PayoutCurvePieceTLV = {
    Random.nextInt(5) match {
      case 0 => linePiece
      case 1 =>
        val outcome =
          Random.between(leftEndpoint.outcome + 1, rightEndpoint.outcome)
        quadraticPiece(outcome)
      case 2 =>
        val outcomeA =
          Random.between(leftEndpoint.outcome + 1, rightEndpoint.outcome)
        @tailrec
        def outcomeB: Long = {
          val outcome =
            Random.between(leftEndpoint.outcome + 1, rightEndpoint.outcome)
          if (outcome == outcomeA) outcomeB
          else outcome
        }
        val Vector(outcome1, outcome2) = Vector(outcomeA, outcomeB).sorted
        cubicPiece(outcome1, outcome2)
      case 3 => inversePiece()
      case 4 => hyperbolaPiece()
    }
  }

  def linearInterpolation(endpoints: Vector[TLVPoint]): PayoutFunctionV0TLV = {
    val pieces = Vector.fill(endpoints.length - 1)(linePiece)
    PayoutFunctionV0TLV(endpoints, pieces)
  }

  def linearInterpolation(numPoints: Int, maxVal: Long): PayoutFunctionV0TLV = {
    linearInterpolation(tlvPoints(numPoints, maxVal))
  }

  def piecewisePolynomial(numPoints: Int, maxVal: Long): PayoutFunctionV0TLV = {
    val points = tlvPoints(numPoints, maxVal)

    val (_, _, midpoints, tailEndpoints) =
      points.tail.foldLeft(
        (false,
         Vector.empty[TLVPoint],
         Vector.empty[Vector[TLVPoint]],
         Vector.empty[TLVPoint])) {
        case ((prevIsMidpoint, currentMidpoints, piecesSoFar, endpointsSoFar),
              point) =>
          val isMidpoint =
            if (point != points.last) Random.nextBoolean()
            else false

          val (newCurrentMidpoints, newPiecesSoFar, newEndpointsSoFar) =
            if (isMidpoint) {
              if (prevIsMidpoint) {
                (currentMidpoints.:+(point), piecesSoFar, endpointsSoFar)
              } else {
                (Vector(point), piecesSoFar, endpointsSoFar)
              }
            } else {
              (Vector.empty,
               piecesSoFar.:+(currentMidpoints),
               endpointsSoFar.:+(point))
            }

          (isMidpoint, newCurrentMidpoints, newPiecesSoFar, newEndpointsSoFar)
      }

    val endpoints = tailEndpoints.+:(points.head)
    val pieces = midpoints.map(PolynomialPayoutCurvePieceTLV(_))
    PayoutFunctionV0TLV(endpoints, pieces)
  }

  def inverseFunction(maxVal: Long): PayoutFunctionV0TLV = {
    val endpoints = Vector(tlvPoint(0), tlvPoint(maxVal))
    PayoutFunctionV0TLV(endpoints, Vector(inversePiece()))
  }

  def hyperbola(maxVal: Long): PayoutFunctionV0TLV = {
    val endpoints = Vector(tlvPoint(0), tlvPoint(maxVal))
    PayoutFunctionV0TLV(endpoints, Vector(hyperbolaPiece()))
  }

  def piecewiseLineAndHyperbola(
      numPoints: Int,
      maxVal: Long): PayoutFunctionV0TLV = {
    val endpoints = tlvPoints(numPoints, maxVal)
    val pieces = endpoints.tail.map { _ =>
      if (Random.nextBoolean()) {
        hyperbolaPiece()
      } else {
        linePiece
      }
    }
    PayoutFunctionV0TLV(endpoints, pieces)
  }

  def payoutFunction(numPoints: Int, maxVal: Long): PayoutFunctionV0TLV = {
    val endpoints = tlvPoints(numPoints, maxVal)
    val pieces = endpoints.init.zip(endpoints.tail).map {
      case (leftEndpoint, rightEndpoint) =>
        randomPiece(leftEndpoint, rightEndpoint)
    }

    PayoutFunctionV0TLV(endpoints, pieces)
  }

  val noRounding: RoundingIntervalsV0TLV = RoundingIntervalsV0TLV.noRounding

  def rounding(numIntervals: Int, maxVal: Long): RoundingIntervalsV0TLV = {
    val intervalStarts =
      0.until(numIntervals).toVector.map(_ => Random.nextLong(maxVal)).sorted
    val intervals = intervalStarts.map { intervalStart =>
      (intervalStart, Satoshis(Random.nextInt().abs))
    }

    RoundingIntervalsV0TLV(intervals)
  }

  def enumOutcomes(size: Int): Vector[String] = {
    DLCTestUtil.genOutcomes(size)
  }

  def enumDescriptor(
      size: Int,
      totalInput: Satoshis): ContractDescriptorV0TLV = {
    val outcomes = enumOutcomes(size)
    val values = DLCTestUtil.genValues(size, totalInput)

    ContractDescriptorV0TLV(outcomes.zip(values))
  }

  // TODO: Generators for other payout functions
  def numericDescriptor(
      numPoints: Int,
      numRoundingIntervals: Int): ContractDescriptorV1TLV = {
    val maxVal = (1L << numDigits) - 1
    ContractDescriptorV1TLV(numDigits,
                            payoutFunction(numPoints, maxVal),
                            rounding(numRoundingIntervals, maxVal))
  }

  def enumEventDescriptor(size: Int): EnumEventDescriptorV0TLV = {
    EnumEventDescriptorV0TLV(enumOutcomes(size))
  }

  def unsignedNumericEventDescriptor(
      unit: String = "test_unit",
      precision: Int32 =
        Int32.zero): UnsignedDigitDecompositionEventDescriptor = {
    UnsignedDigitDecompositionEventDescriptor(base = UInt16(2),
                                              UInt16(numDigits),
                                              unit,
                                              precision)
  }

  def signedNumericEventDescriptor(
      unit: String = "test_unit",
      precision: Int32 =
        Int32.zero): SignedDigitDecompositionEventDescriptor = {
    SignedDigitDecompositionEventDescriptor(base = UInt16(2),
                                            UInt16(numDigits),
                                            unit,
                                            precision)
  }

  def nonces(numNonces: Int = numDigits): Vector[SchnorrNonce] = {
    0.until(numNonces)
      .toVector
      .map(_ => ECPrivateKey.freshPrivateKey.schnorrNonce)
  }

  def oracleEvent(
      descriptor: EventDescriptorTLV,
      nonces: Vector[SchnorrNonce] = this.nonces(),
      eventMaturity: UInt32 = UInt32.zero,
      eventId: String = "test_event"): OracleEventV0TLV = {
    OracleEventV0TLV(nonces, eventMaturity, descriptor, eventId)
  }

  def enumOracleEvent(size: Int): OracleEventV0TLV = {
    oracleEvent(enumEventDescriptor(size))
  }

  def unsignedNumericOracleEvent(): OracleEventV0TLV = {
    oracleEvent(unsignedNumericEventDescriptor())
  }

  def signedNumericOracleEvent(): OracleEventV0TLV = {
    oracleEvent(signedNumericEventDescriptor())
  }

  def oracleAnnouncement(
      oracleEvent: OracleEventV0TLV,
      privKey: ECPrivateKey =
        ECPrivateKey.freshPrivateKey): OracleAnnouncementV0TLV = {
    val sig =
      privKey.schnorrSign(
        CryptoUtil.sha256DLCAnnouncement(oracleEvent.bytes).bytes)

    OracleAnnouncementV0TLV(sig, privKey.schnorrPublicKey, oracleEvent)
  }

  def enumOracleAnnouncement(size: Int): OracleAnnouncementV0TLV = {
    oracleAnnouncement(enumOracleEvent(size))
  }

  def unsignedNumericOracleAnnouncement(): OracleAnnouncementV0TLV = {
    oracleAnnouncement(unsignedNumericOracleEvent())
  }

  def signedNumericOracleAnnouncement(): OracleAnnouncementV0TLV = {
    oracleAnnouncement(signedNumericOracleEvent())
  }

  def enumOracleAttestment(): OracleAttestmentV0TLV = {
    ???
  }

  def unsignedNumericOracleAttestment(): OracleAttestmentV0TLV = {
    ???
  }

  def signedNumericOracleAttestment(): OracleAttestmentV0TLV = {
    ???
  }

  def singleOracleInfo(
      announcement: OracleAnnouncementV0TLV): OracleInfoV0TLV = {
    OracleInfoV0TLV(announcement)
  }

  def enumSingleOracleInfo(size: Int): OracleInfoV0TLV = {
    singleOracleInfo(enumOracleAnnouncement(size))
  }

  def unsignedNumericSingleOracleInfo(): OracleInfoV0TLV = {
    singleOracleInfo(unsignedNumericOracleAnnouncement())
  }

  def signedNumericSingleOracleInfo(): OracleInfoV0TLV = {
    singleOracleInfo(signedNumericOracleAnnouncement())
  }

  def enumMultiOracleInfo(
      size: Int,
      threshold: Int,
      numOracles: Int): OracleInfoV1TLV = {
    val oracles =
      0.until(numOracles).toVector.map(_ => enumOracleAnnouncement(size))
    OracleInfoV1TLV(threshold, oracles)
  }

  def unsignedNumericExactMultiOracleInfo(
      threshold: Int,
      numOracles: Int): OracleInfoV1TLV = {
    val oracles =
      0.until(numOracles).toVector.map(_ => unsignedNumericOracleAnnouncement())
    OracleInfoV1TLV(threshold, oracles)
  }

  def signedNumericExactMultiOracleInfo(
      threshold: Int,
      numOracles: Int): OracleInfoV1TLV = {
    val oracles =
      0.until(numOracles).toVector.map(_ => signedNumericOracleAnnouncement())
    OracleInfoV1TLV(threshold, oracles)
  }

  def oracleParams(
      maxErrorExp: Int = 10,
      minFailExp: Int = 5,
      maximizeCoverage: Boolean = Random.nextBoolean()): OracleParamsV0TLV = {
    OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage)
  }

  def unsignedNumericMultiOracleInfo(
      threshold: Int,
      numOracles: Int,
      params: OracleParamsV0TLV = oracleParams()): OracleInfoV2TLV = {
    val oracles =
      0.until(numOracles).toVector.map(_ => unsignedNumericOracleAnnouncement())
    OracleInfoV2TLV(threshold, oracles, params)
  }

  def signedNumericMultiOracleInfo(
      threshold: Int,
      numOracles: Int,
      params: OracleParamsV0TLV = oracleParams()): OracleInfoV2TLV = {
    val oracles =
      0.until(numOracles).toVector.map(_ => signedNumericOracleAnnouncement())
    OracleInfoV2TLV(threshold, oracles, params)
  }

  // TODO All places using size must also use outcomes to allow descriptor and oracle info to match
  def enumSingleOracleContractInfo(
      size: Int,
      totalCollateral: Satoshis): ContractInfoV0TLV = {
    ContractInfoV0TLV(totalCollateral,
                      enumDescriptor(size, totalCollateral),
                      enumSingleOracleInfo(size))
  }

  def enumMultiOracleContractInfo(
      size: Int,
      totalCollateral: Satoshis,
      threshold: Int,
      numOracles: Int): ContractInfoV0TLV = {
    ContractInfoV0TLV(totalCollateral,
                      enumDescriptor(size, totalCollateral),
                      enumMultiOracleInfo(size, threshold, numOracles))
  }

  def numericSingleOracleContractInfo(
      totalCollateral: Satoshis,
      numPoints: Int,
      numRoundingIntervals: Int): ContractInfoV0TLV = {
    ContractInfoV0TLV(totalCollateral,
                      numericDescriptor(numPoints, numRoundingIntervals),
                      unsignedNumericSingleOracleInfo())
  }

  def numericExactMultiOracleContractInfo(
      totalCollateral: Satoshis,
      numPoints: Int,
      numRoundingIntervals: Int,
      threshold: Int,
      numOracles: Int): ContractInfoV0TLV = {
    ContractInfoV0TLV(
      totalCollateral,
      numericDescriptor(numPoints, numRoundingIntervals),
      unsignedNumericExactMultiOracleInfo(threshold, numOracles))
  }

  def numericMultiOracleContractInfo(
      totalCollateral: Satoshis,
      numPoints: Int,
      numRoundingIntervals: Int,
      threshold: Int,
      numOracles: Int,
      oracleParams: OracleParamsV0TLV =
        this.oracleParams()): ContractInfoV0TLV = {
    ContractInfoV0TLV(
      totalCollateral,
      numericDescriptor(numPoints, numRoundingIntervals),
      unsignedNumericMultiOracleInfo(threshold, numOracles, oracleParams))
  }

  def disjointUnionContractInfo(
      contracts: Vector[ContractInfoV0TLV]): ContractInfoV1TLV = {
    val contractOraclePairs = contracts.map { contract =>
      (contract.contractDescriptor, contract.oracleInfo)
    }
    ContractInfoV1TLV(contracts.head.totalCollateral, contractOraclePairs)
  }

  def address(
      spk: ScriptPubKey = p2wpkh(),
      network: NetworkParameters = RegTest): BitcoinAddress = {
    spk match {
      case wspk: WitnessScriptPubKey => Bech32Address(wspk, network)
      case p2sh: P2SHScriptPubKey    => P2SHAddress(p2sh, network)
      case p2pkh: P2PKHScriptPubKey  => P2PKHAddress(p2pkh, network)
      case _: RawScriptPubKey =>
        throw new IllegalArgumentException(s"$spk is not valid for an address")
    }
  }

  def inputTransaction(
      input: CurrencyUnit = defaultAmt,
      spk: ScriptPubKey = p2wpkh()): Transaction = {
    BaseTransaction(
      TransactionConstants.validLockVersion,
      Vector.empty,
      Vector(TransactionOutput(input * 2, spk)),
      UInt32.zero
    )
  }

  def fundingInput(
      inputSerialId: UInt64 = DLCMessage.genSerialId(),
      prevTx: Transaction = inputTransaction(),
      prevTxVout: UInt32 = UInt32.zero,
      sequence: UInt32 = TransactionConstants.sequence,
      maxWitnessLen: UInt16 = UInt16(107),
      redeemScriptOpt: Option[WitnessScriptPubKey] = None): DLCFundingInput = {
    DLCFundingInput(inputSerialId,
                    prevTx,
                    prevTxVout,
                    sequence,
                    maxWitnessLen,
                    redeemScriptOpt)
  }

  def p2wpkh(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey): P2WPKHWitnessSPKV0 = {
    P2WPKHWitnessSPKV0(pubKey)
  }

  def multiSig(threshold: Int, total: Int): MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(threshold,
                               Vector.fill(total)(ECPublicKey.freshPublicKey))
  }

  def p2wsh(threshold: Int, total: Int): P2WSHWitnessSPKV0 = {
    P2WSHWitnessSPKV0(multiSig(threshold, total))
  }

  def p2sh(nested: ScriptPubKey): P2SHScriptPubKey = {
    P2SHScriptPubKey(nested)
  }

  def p2wpkhFundingInput(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      inputSerialId: UInt64 = DLCMessage.genSerialId(),
      prevTx: Transaction = inputTransaction(),
      prevTxVout: UInt32 = UInt32.zero,
      sequence: UInt32 = TransactionConstants.sequence): FundingInputV0TLV = {
    val info = P2WPKHV0InputInfo(TransactionOutPoint(prevTx.txId, prevTxVout),
                                 prevTx.outputs(prevTxVout.toInt).value,
                                 pubKey)
    val maxWitnessLen = InputInfo.maxWitnessLen(info)
    fundingInput(inputSerialId,
                 prevTx,
                 prevTxVout,
                 sequence,
                 UInt16(maxWitnessLen),
                 redeemScriptOpt = None).toTLV
  }

  def p2wshFundingInput(
      threshold: Int,
      total: Int,
      inputSerialId: UInt64 = DLCMessage.genSerialId(),
      prevTx: Transaction = inputTransaction(),
      prevTxVout: UInt32 = UInt32.zero,
      sequence: UInt32 = TransactionConstants.sequence): FundingInputV0TLV = {
    val nested = multiSig(threshold, total)
    val info = P2WSHV0InputInfo(TransactionOutPoint(prevTx.txId, prevTxVout),
                                prevTx.outputs(prevTxVout.toInt).value,
                                P2WSHWitnessV0(nested),
                                ConditionalPath.NoCondition)
    val maxWitnessLen = InputInfo.maxWitnessLen(info)
    fundingInput(inputSerialId,
                 prevTx,
                 prevTxVout,
                 sequence,
                 UInt16(maxWitnessLen),
                 redeemScriptOpt = None).toTLV
  }

  def p2shP2WPKHFundingInput(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      inputSerialId: UInt64 = DLCMessage.genSerialId(),
      prevTx: Transaction = inputTransaction(),
      prevTxVout: UInt32 = UInt32.zero,
      sequence: UInt32 = TransactionConstants.sequence): FundingInputV0TLV = {
    val info = P2SHNestedSegwitV0InputInfo(
      TransactionOutPoint(prevTx.txId, prevTxVout),
      prevTx.outputs(prevTxVout.toInt).value,
      P2WPKHWitnessV0(pubKey),
      ConditionalPath.NoCondition)
    val maxWitnessLen = InputInfo.maxWitnessLen(info)
    fundingInput(inputSerialId,
                 prevTx,
                 prevTxVout,
                 sequence,
                 UInt16(maxWitnessLen),
                 redeemScriptOpt = Some(p2wpkh(pubKey))).toTLV
  }

  def p2shP2WSHFundingInput(
      threshold: Int,
      total: Int,
      inputSerialId: UInt64 = DLCMessage.genSerialId(),
      prevTx: Transaction = inputTransaction(),
      prevTxVout: UInt32 = UInt32.zero,
      sequence: UInt32 = TransactionConstants.sequence): FundingInputV0TLV = {
    val nested = multiSig(threshold, total)
    val info = P2SHNestedSegwitV0InputInfo(
      TransactionOutPoint(prevTx.txId, prevTxVout),
      prevTx.outputs(prevTxVout.toInt).value,
      P2WSHWitnessV0(nested),
      ConditionalPath.NoCondition)
    val maxWitnessLen = InputInfo.maxWitnessLen(info)
    fundingInput(inputSerialId,
                 prevTx,
                 prevTxVout,
                 sequence,
                 UInt16(maxWitnessLen),
                 redeemScriptOpt = Some(P2WSHWitnessSPKV0(nested))).toTLV
  }

  def adaptorSig: ECAdaptorSignature = {
    ECAdaptorSignature(
      ECPublicKey.freshPublicKey,
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPrivateKey.freshPrivateKey.fieldElement
    )
  }

  def cetSigs(numSigs: Int): CETSignaturesV0TLV = {
    CETSignaturesV0TLV(Vector.fill(numSigs)(adaptorSig))
  }

  def p2wpkhFundingSig(): FundingSignaturesV0TLV = {
    ???
  }

  def p2wshFundingSigs(): FundingSignaturesV0TLV = {
    ???
  }

  def dlcOffer(): DLCOfferTLV = {
    ???
  }

  def hash(bytes: ByteVector = NumberUtil.randomBytes(32)): Sha256Digest = {
    CryptoUtil.sha256(bytes)
  }

  def dlcAccept(): DLCAcceptTLV = {
    ???
  }

  def dlcAcceptFromOffer(offer: DLCOfferTLV): DLCAcceptTLV = {
    ???
  }

  def outputReference(
      input: CurrencyUnit = defaultAmt,
      spk: ScriptPubKey =
        P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)): OutputReference = {
    val tx = inputTransaction(input, spk)
    OutputReference(TransactionOutPoint(tx.txIdBE, UInt32.zero),
                    tx.outputs.head)
  }

  def ecdsaSig(sigHashByte: Boolean = true): ECDigitalSignature = {
    val sigWithoutSigHash = ECDigitalSignature.fromRS(
      ECPrivateKey.freshPrivateKey.fieldElement.toBigInteger,
      ECPrivateKey.freshPrivateKey.fieldElement.toBigInteger)

    if (sigHashByte) {
      ECDigitalSignature(sigWithoutSigHash.bytes :+ 0x01)
    } else {
      sigWithoutSigHash
    }
  }

  def partialSig(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      sigHashByte: Boolean = true): PartialSignature = {
    PartialSignature(pubKey, ecdsaSig(sigHashByte))
  }

  def p2wpkhWitnessV0(
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey,
      sigHashByte: Boolean = true): P2WPKHWitnessV0 = {
    P2WPKHWitnessV0(pubKey, ecdsaSig(sigHashByte))
  }

  def fundingSigs(
      outPoints: Vector[TransactionOutPoint] = Vector(
        outputReference().outPoint)): FundingSignatures = {
    FundingSignatures(outPoints.map(outpoint => outpoint -> p2wpkhWitnessV0()))
  }

  def dlcSign(): DLCSignTLV = {
    ???
  }

  def dlcSignTLVFromOffer(offer: DLCOfferTLV): DLCSignTLV = {
    ???
  }

  def dlcSignFromOfferAndAccept(
      offer: DLCOfferTLV,
      accept: DLCAcceptTLV): DLCSignTLV = {
    ???
  }
}
