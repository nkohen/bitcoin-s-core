package org.bitcoins.dlc.testgen2

import org.bitcoins.core.config.{NetworkParameters, RegTest}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
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
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.util.Random

// TODO: Worry about Random.nextDouble being un-bounded, probably introduce var
object DLCTLVGen {

  val defaultAmt: Satoshis = CurrencyUnits.oneBTC.satoshis

  def tlvPoint(
      outcome: Long,
      value: Double = Random.nextDouble().abs): TLVPoint = {
    val roundedPayout = Satoshis(value.toLong)
    val extraPrecision = (value - roundedPayout.toLong) * (1 << 16)
    TLVPoint(outcome, roundedPayout, extraPrecision.toInt)
  }

  def tlvPoints(numPoints: Int, maxVal: Long): Vector[TLVPoint] = {
    0.until(numPoints)
      .toVector
      .map { _ =>
        tlvPoint(Random.nextLong(maxVal))
      }
      .distinctBy(_.outcome)
      .sortBy(_.outcome)
  }

  def signed16PTLVNumber(
      number: Double = Random.nextDouble()): Signed16PTLVNumber = {
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
      value: Double =
        Random.nextDouble().abs): PolynomialPayoutCurvePieceTLV = {
    PolynomialPayoutCurvePieceTLV(Vector(tlvPoint(outcome, value)))
  }

  def cubicPiece(
      outcome1: Long,
      outcome2: Long,
      value1: Double = Random.nextDouble().abs,
      value2: Double =
        Random.nextDouble().abs): PolynomialPayoutCurvePieceTLV = {
    PolynomialPayoutCurvePieceTLV(
      Vector(tlvPoint(outcome1, value1), tlvPoint(outcome2, value2)))
  }

  def inversePiece(
      translatePayout: Double = Random.nextDouble(),
      d: Double = Random.nextDouble()): HyperbolaPayoutCurvePieceTLV = {
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
      translateOutcome: Double = Random.nextDouble(),
      translatePayout: Double = Random.nextDouble(),
      a: Double = Random.nextDouble(),
      b: Double = Random.nextDouble(),
      c: Double = Random.nextDouble(),
      d: Double = Random.nextDouble()): HyperbolaPayoutCurvePieceTLV = {
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

  def linearInterpolation(endpoints: Vector[TLVPoint]): PayoutFunctionV0TLV = {
    val pieces = Vector.fill(endpoints.length)(linePiece)
    PayoutFunctionV0TLV(endpoints, pieces)
  }

  def linearInterpolation(numPoints: Int, maxVal: Long): PayoutFunctionV0TLV = {
    linearInterpolation(tlvPoints(numPoints, maxVal))
  }

  def piecewisePolynomial(): PayoutFunctionV0TLV = {
    ???
  }

  def inverseFunction(): PayoutFunctionV0TLV = {
    ???
  }

  def hyperbola(): PayoutFunctionV0TLV = {
    ???
  }

  def piecewiseLineAndHyperbola(): PayoutFunctionV0TLV = {
    ???
  }

  def payoutFunction(): PayoutFunctionV0TLV = {
    ???
  }

  val noRounding: RoundingIntervalsV0TLV = RoundingIntervalsV0TLV.noRounding

  def rounding(): RoundingIntervalsV0TLV = {
    ???
  }

  def enumDescriptor(): ContractDescriptorV0TLV = {
    ???
  }

  def numericDescriptor(): ContractDescriptorV1TLV = {
    ???
  }

  def enumEventDescriptor(): EnumEventDescriptorV0TLV = {
    ???
  }

  def unsignedNumericEventDescriptor(): UnsignedDigitDecompositionEventDescriptor = {
    ???
  }

  def signedNumericEventDescriptor(): SignedDigitDecompositionEventDescriptor = {
    ???
  }

  def oracleEvent(): OracleEventV0TLV = {
    ???
  }

  def enumOracleEvent(): OracleEventV0TLV = {
    ???
  }

  def unsignedNumericOracleEvent(): OracleEventV0TLV = {
    ???
  }

  def signedNumericOracleEvent(): OracleEventV0TLV = {
    ???
  }

  def oracleAnnouncement(): OracleAnnouncementV0TLV = {
    ???
  }

  def enumOracleAnnouncement(): OracleAnnouncementV0TLV = {
    ???
  }

  def unsignedNumericOracleAnnouncement(): OracleAnnouncementV0TLV = {
    ???
  }

  def signedNumericOracleAnnouncement(): OracleAnnouncementV0TLV = {
    ???
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

  def singleOracleInfo(): OracleInfoV0TLV = {
    ???
  }

  def enumSingleOracleInfo(): OracleInfoV0TLV = {
    ???
  }

  def unsignedNumericSingleOracleInfo(): OracleInfoV0TLV = {
    ???
  }

  def signedNumericSingleOracleInfo(): OracleInfoV0TLV = {
    ???
  }

  def enumMultiOracleInfo(): OracleInfoV1TLV = {
    ???
  }

  def unsignedNumericExactMultiOracleInfo(): OracleInfoV1TLV = {
    ???
  }

  def signedNumericExactMultiOracleInfo(): OracleInfoV1TLV = {
    ???
  }

  def oracleParams(): OracleParamsV0TLV = {
    ???
  }

  def unsignedNumericMultiOracleInfo(): OracleInfoV2TLV = {
    ???
  }

  def signedNumericMultiOracleInfo(): OracleInfoV2TLV = {
    ???
  }

  def enumSingleOracleContractInfo(): ContractInfoV0TLV = {
    ???
  }

  def enumMultiOracleContractInfo(): ContractInfoV0TLV = {
    ???
  }

  def unsignedNumericSingleOracleContractInfo(): ContractInfoV0TLV = {
    ???
  }

  def numericExactMultiOracleContractInfo(): ContractInfoV0TLV = {
    ???
  }

  def numericMultiOracleContractInfo(): ContractInfoV0TLV = {
    ???
  }

  def disjointUnionContractInfo(): ContractInfoV1TLV = {
    ???
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

  def p2wpkhFundingInput(): FundingInputV0TLV = {
    ???
  }

  def p2wshFundingInput(): FundingInputV0TLV = {
    ???
  }

  def p2shP2WPKHFundingInput(): FundingInputV0TLV = {
    ???
  }

  def p2shP2WSHFundingInput(): FundingInputV0TLV = {
    ???
  }

  def adaptorSig: ECAdaptorSignature = {
    ECAdaptorSignature(
      ECPublicKey.freshPublicKey,
      ECPrivateKey.freshPrivateKey.fieldElement,
      ECPublicKey.freshPublicKey,
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
