package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.config.{NetworkParameters, Networks}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{ScriptWitnessV0, WitnessScriptPubKey}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.protocol.{BigSizeUInt, BitcoinAddress}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.serializers.script.RawScriptWitnessParser
import org.bitcoins.core.util.{MapWrapper, SeqWrapper}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import scodec.bits.ByteVector
import ujson._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

sealed trait DLCMessage {
  def toJson: Value
  def toJsonStr: String = toJson.toString()
}

object DLCMessage {

  def calcParamHash(
      oracleInfo: OracleInfo[_],
      contractInfo: ContractInfo[_],
      timeouts: DLCTimeouts): Sha256DigestBE = {
    CryptoUtil
      .sha256(oracleInfo.bytes ++ contractInfo.bytes ++ timeouts.bytes)
      .flip
  }

  private def getValue(key: String)(implicit
      obj: mutable.LinkedHashMap[String, Value]): Value = {
    val index = obj.keys.toList.indexOf(key)
    obj.values(index)
  }

  sealed trait OracleInfo[Outcome] extends NetworkElement {
    def pubKey: SchnorrPublicKey
    def nonces: Vector[SchnorrNonce]
    def toTLV: OracleInfoTLV[Outcome]

    def verifySigs(
        outcome: Outcome,
        sigs: Vector[SchnorrDigitalSignature]): Boolean
  }

  case class SingleNonceOracleInfo(
      pubKey: SchnorrPublicKey,
      rValue: SchnorrNonce)
      extends OracleInfo[Sha256Digest] {

    override def nonces: Vector[SchnorrNonce] = Vector(rValue)

    override def bytes: ByteVector = pubKey.bytes ++ rValue.bytes

    override def toTLV: OracleInfoV0TLV = OracleInfoV0TLV(pubKey, rValue)

    override def verifySigs(
        outcome: Sha256Digest,
        sigs: Vector[SchnorrDigitalSignature]): Boolean = {
      sigs.length == 1 && pubKey.verify(outcome.bytes, sigs.head)
    }
  }

  object SingleNonceOracleInfo extends Factory[SingleNonceOracleInfo] {

    override def fromBytes(bytes: ByteVector): SingleNonceOracleInfo = {
      require(bytes.size == 64,
              s"SingleNonceOracleInfo is only 64 bytes, got $bytes")

      val pubkey = SchnorrPublicKey(bytes.take(32))
      val rValue = SchnorrNonce(bytes.drop(32))

      SingleNonceOracleInfo(pubkey, rValue)
    }
  }

  case class MultiNonceOracleInfo(
      pubKey: SchnorrPublicKey,
      base: Int,
      nonces: Vector[SchnorrNonce])
      extends OracleInfo[Vector[Int]] {

    override def bytes: ByteVector = {
      pubKey.bytes ++ UInt16(base).bytes ++
        nonces.foldLeft(ByteVector.empty)(_ ++ _.bytes)
    }

    override def toTLV: OracleInfoTLV[Vector[Int]] = ???

    override def verifySigs(
        outcome: Vector[Int],
        sigs: Vector[SchnorrDigitalSignature]): Boolean = {
      outcome.zip(sigs.take(outcome.length)).forall {
        case (num, sig) =>
          pubKey.verify(BigSizeUInt(num).bytes, sig)
      }
    }
  }

  object MultiNonceOracleInfo extends Factory[MultiNonceOracleInfo] {

    override def fromBytes(bytes: ByteVector): MultiNonceOracleInfo = {
      val numNonces = (bytes.length - 34) / 32

      require(bytes.length - 34 == 32 * numNonces && numNonces > 0,
              s"$bytes was not a valid size for MultiNonceOracleInfo")

      val pubKey = SchnorrPublicKey(bytes.take(32))
      val base = UInt16(bytes.drop(32).take(2)).toInt
      val bytesLeft = bytes.drop(34)
      val nonces = 0L.until(numNonces).foldLeft(Vector.empty[SchnorrNonce]) {
        case (nonces, index) =>
          nonces :+ SchnorrNonce(bytesLeft.drop(index * 32).take(32))
      }

      MultiNonceOracleInfo(pubKey, base, nonces)
    }
  }

  object OracleInfo extends Factory[OracleInfo[_]] {

    val dummy: OracleInfo[_] = OracleInfo(ByteVector.fill(64)(1))

    override def fromBytes(bytes: ByteVector): OracleInfo[_] = {
      if (bytes.length == 64) {
        SingleNonceOracleInfo(bytes)
      } else {
        MultiNonceOracleInfo(bytes)
      }
    }

    def fromTLV[T](oracleInfo: OracleInfoTLV[T]): OracleInfo[T] = {
      oracleInfo match {
        case OracleInfoV0TLV(pubKey, rValue) =>
          SingleNonceOracleInfo(pubKey, rValue).asInstanceOf[OracleInfo[T]]
      }
    }

    def apply(
        pubKey: SchnorrPublicKey,
        rValue: SchnorrNonce): SingleNonceOracleInfo = {
      SingleNonceOracleInfo(pubKey, rValue)
    }

    def apply(
        pubKey: SchnorrPublicKey,
        base: Int,
        nonces: Vector[SchnorrNonce]): MultiNonceOracleInfo = {
      MultiNonceOracleInfo(pubKey, base, nonces)
    }
  }

  sealed trait ContractInfo[Outcome] extends NetworkElement {
    def toTLV: ContractInfoTLV[Outcome]

    def toJson: Value

    def flip(totalCollateral: Satoshis): ContractInfo[Outcome]

    def apply(outcome: Outcome): Satoshis
  }

  case class SingleNonceContractInfo(
      outcomeValueMap: Map[Sha256Digest, Satoshis])
      extends ContractInfo[Sha256Digest]
      with MapWrapper[Sha256Digest, Satoshis] {
    override def wrapped: Map[Sha256Digest, Satoshis] = outcomeValueMap

    override def bytes: ByteVector = {
      outcomeValueMap.foldLeft(ByteVector.empty) {
        case (vec, (digest, sats)) => vec ++ digest.bytes ++ sats.bytes
      }
    }

    override def toTLV: ContractInfoV0TLV = ContractInfoV0TLV(outcomeValueMap)

    override def toJson: Value = {
      outcomeValueMap.map(info =>
        mutable.LinkedHashMap("sha256" -> Str(info._1.hex),
                              "sats" -> Num(info._2.toLong.toDouble)))
    }

    override def flip(totalCollateral: Satoshis): SingleNonceContractInfo = {
      SingleNonceContractInfo(outcomeValueMap.map {
        case (hash, amt) => (hash, (totalCollateral - amt).satoshis)
      })
    }
  }

  object SingleNonceContractInfo extends Factory[SingleNonceContractInfo] {

    private val sizeOfMapElement: Int = 40

    val empty: ContractInfo[Sha256Digest] = SingleNonceContractInfo(
      ByteVector.low(sizeOfMapElement))

    override def fromBytes(bytes: ByteVector): SingleNonceContractInfo = {
      @tailrec
      def loop(
          remainingBytes: ByteVector,
          accum: Vector[(Sha256Digest, Satoshis)]): Vector[
        (Sha256Digest, Satoshis)] = {
        if (remainingBytes.size < sizeOfMapElement) {
          accum
        } else {
          val relevantBytes = remainingBytes.take(sizeOfMapElement)
          val digest = Sha256Digest(relevantBytes.take(32))
          val sats = Satoshis(relevantBytes.takeRight(8))
          loop(remainingBytes.drop(sizeOfMapElement), accum :+ (digest, sats))
        }
      }
      SingleNonceContractInfo(loop(bytes, Vector.empty).toMap)
    }
  }

  case class MultiNonceContractInfoPoint(
      outcome: Long,
      payout: Satoshis,
      isEndpoint: Boolean)
      extends NetworkElement {

    override def bytes: ByteVector = {
      val isEndpointByte =
        if (isEndpoint) 1.toByte
        else 0.toByte

      BigSizeUInt(outcome).bytes ++
        payout.bytes ++
        ByteVector.fromByte(isEndpointByte)
    }
  }

  object MultiNonceContractInfoPoint
      extends Factory[MultiNonceContractInfoPoint] {

    override def fromBytes(bytes: ByteVector): MultiNonceContractInfoPoint = {
      val outcome = BigSizeUInt(bytes)
      val payout = Satoshis(bytes.drop(outcome.byteSize).take(8))
      val isEndpoint =
        if (bytes.drop(outcome.byteSize + 8).head == 0.toByte) false
        else true

      MultiNonceContractInfoPoint(outcome.toLong, payout, isEndpoint)
    }
  }

  case class MultiNonceContractInfo(points: Vector[MultiNonceContractInfoPoint])
      extends ContractInfo[Vector[Int]]
      with SeqWrapper[MultiNonceContractInfoPoint] {
    override def wrapped: Vector[MultiNonceContractInfoPoint] = points

    override def bytes: ByteVector =
      points.foldLeft(ByteVector.empty)(_ ++ _.bytes)

    override def toTLV: ContractInfoTLV[Vector[Int]] = ???

    override def toJson: Value = {
      points.map(info =>
        mutable.LinkedHashMap("outcome" -> Num(info.outcome.toDouble),
                              "payout" -> Num(info.payout.toLong.toDouble),
                              "isEndpoint" -> Bool(info.isEndpoint)))
    }

    override def flip(totalCollateral: Satoshis): MultiNonceContractInfo = {
      MultiNonceContractInfo(points.map(point =>
        point.copy(payout = (totalCollateral - point.payout).satoshis)))
    }

    override def apply(outcome: Vector[Int]): Satoshis = ???
  }

  object MultiNonceContractInfo extends Factory[MultiNonceContractInfo] {

    override def fromBytes(bytes: ByteVector): MultiNonceContractInfo = {
      @tailrec
      def loop(
          remainingBytes: ByteVector,
          accum: Vector[MultiNonceContractInfoPoint]): Vector[
        MultiNonceContractInfoPoint] = {
        if (remainingBytes.isEmpty) {
          accum
        } else {
          val nextPoint = MultiNonceContractInfoPoint(remainingBytes)
          loop(remainingBytes.drop(nextPoint.byteSize), accum :+ nextPoint)
        }
      }

      MultiNonceContractInfo(loop(bytes, Vector.empty))
    }

    def longCollaredFuture(
        largestZeroOutcome: Long,
        smallestTotalOutcome: Long,
        maxOutcome: Long,
        totalCollateral: Satoshis): MultiNonceContractInfo = {
      MultiNonceContractInfo(
        Vector(
          MultiNonceContractInfoPoint(0, Satoshis.zero, isEndpoint = true),
          MultiNonceContractInfoPoint(largestZeroOutcome,
                                      Satoshis.zero,
                                      isEndpoint = true),
          MultiNonceContractInfoPoint(smallestTotalOutcome,
                                      totalCollateral,
                                      isEndpoint = true),
          MultiNonceContractInfoPoint(maxOutcome,
                                      totalCollateral,
                                      isEndpoint = true)
        ))
    }

    def shortCollaredFuture(
        largestTotalOutcome: Long,
        smallestZeroOutcome: Long,
        maxOutcome: Long,
        totalCollateral: Satoshis): MultiNonceContractInfo = {
      MultiNonceContractInfo(
        Vector(
          MultiNonceContractInfoPoint(0, totalCollateral, isEndpoint = true),
          MultiNonceContractInfoPoint(largestTotalOutcome,
                                      totalCollateral,
                                      isEndpoint = true),
          MultiNonceContractInfoPoint(smallestZeroOutcome,
                                      Satoshis.zero,
                                      isEndpoint = true),
          MultiNonceContractInfoPoint(maxOutcome,
                                      Satoshis.zero,
                                      isEndpoint = true)
        ))
    }
  }

  object ContractInfo extends Factory[ContractInfo[_]] {

    override def fromBytes(bytes: ByteVector): ContractInfo[_] = {
      Try(MultiNonceContractInfo(bytes)) match {
        case Failure(_)            => SingleNonceContractInfo(bytes)
        case Success(contractInfo) => contractInfo
      }
    }

    def fromTLV[T](contractInfo: ContractInfoTLV[T]): ContractInfo[T] = {
      contractInfo match {
        case ContractInfoV0TLV(outcomes) =>
          SingleNonceContractInfo(outcomes).asInstanceOf[ContractInfo[T]]
      }
    }

    def fromJson(json: Value): ContractInfo[_] = {
      json.arr.head.obj.get("isEndpoint") match {
        case Some(_) =>
          val points = json.arr.map { subVal =>
            implicit val obj: mutable.LinkedHashMap[String, Value] = subVal.obj

            val outcome = getValue("outcome")
            val payout = getValue("payout")
            val isEndpoint = getValue("isEndpoint")

            MultiNonceContractInfoPoint(outcome.num.toLong,
                                        Satoshis(payout.num.toLong),
                                        isEndpoint.bool)
          }.toVector

          MultiNonceContractInfo(points)
        case None =>
          val outcomeMap = json.arr.map { subVal =>
            implicit val obj: mutable.LinkedHashMap[String, Value] =
              subVal.obj

            val sha256 = getValue("sha256")
            val sats = getValue("sats")

            (Sha256Digest(sha256.str), Satoshis(sats.num.toLong))
          }.toMap

          SingleNonceContractInfo(outcomeMap)
      }
    }
  }

  sealed trait OracleAndContractInfo[Outcome] {
    def oracleInfo: OracleInfo[Outcome]
    def contractInfo: ContractInfo[Outcome]

    def resultOfOutcome(outcome: Outcome): (ECPublicKey, Satoshis)
    def outcomeFromSignatures(sigs: Vector[SchnorrDigitalSignature]): Outcome
    def allOutcomes: Vector[Outcome]
  }

  case class SingleNonceOracleAndContractInfo(
      oracleInfo: SingleNonceOracleInfo,
      contractInfo: SingleNonceContractInfo)
      extends OracleAndContractInfo[Sha256Digest] {

    lazy val sigPubKeys: Map[Sha256Digest, ECPublicKey] =
      contractInfo.keys.map { msg =>
        msg -> oracleInfo.pubKey.computeSigPoint(msg.bytes, oracleInfo.rValue)
      }.toMap

    override def resultOfOutcome(
        outcome: Sha256Digest): (ECPublicKey, Satoshis) = {
      (sigPubKeys(outcome), contractInfo(outcome))
    }

    override def outcomeFromSignatures(
        sigs: Vector[SchnorrDigitalSignature]): Sha256Digest = {
      sigPubKeys.find(_._2 == sigs.head.sig.getPublicKey) match {
        case Some((hash, _)) => hash
        case None =>
          throw new IllegalArgumentException(
            s"Signature does not correspond to a possible outcome! ${sigs.head}")
      }
    }

    override def allOutcomes: Vector[Sha256Digest] =
      contractInfo.outcomeValueMap.toVector.map(_._1)
  }

  case class MultiNonceOracleAndContractInfo(
      oracleInfo: MultiNonceOracleInfo,
      contractInfo: MultiNonceContractInfo)
      extends OracleAndContractInfo[Vector[Int]] {

    override def resultOfOutcome(
        outcome: Vector[Int]): (ECPublicKey, Satoshis) = {
      ???
    }

    override def outcomeFromSignatures(
        sigs: Vector[SchnorrDigitalSignature]): Vector[Int] = {
      ???
    }

    override def allOutcomes: Vector[Vector[Int]] = {
      ???
    }
  }

  object OracleAndContractInfo {

    def apply[T](
        oracleInfo: OracleInfo[_],
        contractInfo: ContractInfo[T]): OracleAndContractInfo[T] = {
      (oracleInfo, contractInfo) match {
        case (oracleSingle: SingleNonceOracleInfo,
              contractSingle: SingleNonceContractInfo) =>
          SingleNonceOracleAndContractInfo(oracleSingle, contractSingle)
            .asInstanceOf[OracleAndContractInfo[T]]
        case (oracleMulti: MultiNonceOracleInfo,
              contractMulti: MultiNonceContractInfo) =>
          MultiNonceOracleAndContractInfo(oracleMulti, contractMulti)
            .asInstanceOf[OracleAndContractInfo[T]]
        case (_: OracleInfo[_], _: ContractInfo[T]) =>
          throw new IllegalArgumentException(
            s"ContractInfo ($contractInfo) must match OracleInfo ($oracleInfo)")
      }
    }
  }

  sealed trait DLCSetupMessage extends DLCMessage {
    def pubKeys: DLCPublicKeys
    def totalCollateral: Satoshis
    def fundingInputs: Vector[DLCFundingInput]
    def changeAddress: BitcoinAddress
    require(
      totalCollateral >= Satoshis.zero,
      s"Cannot have a negative totalCollateral, got: ${totalCollateral.toLong}")
  }

  /**
    * The initiating party starts the protocol by sending an offer message to the other party.
    *
    * @param contractInfo Contract information consists of a map to be used to create CETs
    * @param oracleInfo The oracle public key and R point(s) to use to build the CETs as
    *                   well as meta information to identify the oracle to be used in the contract.
    * @param pubKeys The relevant public keys that the initiator will be using
    * @param totalCollateral How much the initiator inputs into the contract.
    * @param fundingInputs The set of UTXOs to be used as input to the fund transaction.
    * @param changeAddress The address to use to send the change for the initiator.
    * @param feeRate The fee rate to be used when computing fees for the different transactions.
    * @param timeouts The set of timeouts for the CETs
    */
  case class DLCOffer[Outcome](
      contractInfo: ContractInfo[Outcome],
      oracleInfo: OracleInfo[Outcome],
      pubKeys: DLCPublicKeys,
      totalCollateral: Satoshis,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
      extends DLCSetupMessage {

    lazy val paramHash: Sha256DigestBE =
      calcParamHash(oracleInfo, contractInfo, timeouts)

    val tempContractId: Sha256Digest =
      CryptoUtil.sha256(toMessage.bytes)

    def toTLV: DLCOfferTLV[Outcome] = {
      val chainHash =
        changeAddress.networkParameters.chainParams.genesisBlock.blockHeader.hash

      DLCOfferTLV(
        contractFlags = 0x00,
        chainHash = chainHash,
        contractInfo.toTLV,
        oracleInfo.toTLV,
        fundingPubKey = pubKeys.fundingKey,
        payoutSPK = pubKeys.payoutAddress.scriptPubKey,
        totalCollateralSatoshis = totalCollateral,
        fundingInputs = fundingInputs.map(_.toTLV),
        changeSPK = changeAddress.scriptPubKey,
        feeRate = feeRate,
        contractMaturityBound = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout
      )
    }

    def toMessage: LnMessage[DLCOfferTLV[Outcome]] = {
      LnMessage(this.toTLV)
    }

    override def toJson: Value = {
      val contractInfosJson = contractInfo.toJson

      val fundingInputsJson =
        fundingInputs.map { input =>
          val obj = mutable.LinkedHashMap(
            "prevTx" -> Str(input.prevTx.hex),
            "prevTxVout" -> Num(input.prevTxVout.toInt),
            "sequence" -> Num(input.sequence.toInt),
            "maxWitnessLength" -> Num(input.maxWitnessLen.toInt)
          )

          input.redeemScriptOpt.foreach { redeemScript =>
            obj.+=("redeemScript" -> Str(redeemScript.hex))
          }

          obj
        }

      val timeoutsJson =
        mutable.LinkedHashMap(
          "contractMaturity" -> Num(
            timeouts.contractMaturity.toUInt32.toLong.toDouble),
          "contractTimeout" -> Num(
            timeouts.contractTimeout.toUInt32.toLong.toDouble)
        )

      val pubKeysJson =
        mutable.LinkedHashMap(
          "fundingKey" -> Str(pubKeys.fundingKey.hex),
          "payoutAddress" -> Str(pubKeys.payoutAddress.value)
        )

      Obj(
        mutable.LinkedHashMap[String, Value](
          "contractInfo" -> contractInfosJson,
          "oracleInfo" -> Str(oracleInfo.hex),
          "pubKeys" -> pubKeysJson,
          "totalCollateral" -> Num(totalCollateral.toLong.toDouble),
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "feeRate" -> Num(feeRate.toLong.toDouble),
          "timeouts" -> timeoutsJson
        )
      )
    }
  }

  object DLCOffer {

    def fromTLV[T](offer: DLCOfferTLV[T]): DLCOffer[T] = {
      val network = Networks.fromChainHash(offer.chainHash.flip)

      val contractInfo = ContractInfo.fromTLV(offer.contractInfo)
      val oracleInfo = OracleInfo.fromTLV(offer.oracleInfo)

      DLCOffer(
        contractInfo = contractInfo,
        oracleInfo = oracleInfo,
        pubKeys = DLCPublicKeys(
          offer.fundingPubKey,
          BitcoinAddress.fromScriptPubKey(offer.payoutSPK, network)),
        totalCollateral = offer.totalCollateralSatoshis,
        fundingInputs = offer.fundingInputs.map {
          case input: FundingInputV0TLV => DLCFundingInput.fromTLV(input)
        },
        changeAddress =
          BitcoinAddress.fromScriptPubKey(offer.changeSPK, network),
        feeRate = offer.feeRate,
        timeouts =
          DLCTimeouts(offer.contractMaturityBound, offer.contractTimeout)
      )
    }

    def fromMessage[T](offer: LnMessage[DLCOfferTLV[T]]): DLCOffer[T] = {
      fromTLV(offer.tlv)
    }

    def fromJson(js: Value): DLCOffer[_] = {
      val vec = js.obj.toVector

      val contractInfo =
        vec
          .find(_._1 == "contractInfo")
          .map { case (_, value) => ContractInfo.fromJson(value) }
          .get

      val fundingInputs =
        vec
          .find(_._1 == "fundingInputs")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val prevTx = Transaction(getValue("prevTx").str)
                val prevTxVout = UInt32(getValue("prevTxVout").num.toInt)
                val sequence = UInt32(getValue("sequence").num.toLong)
                val maxWitnessLen =
                  UInt16(getValue("maxWitnessLength").num.toInt)
                val redeemScriptOpt = obj.find(_._1 == "redeemScript").map {
                  case (_, redeemScript) =>
                    WitnessScriptPubKey(redeemScript.str)
                }

                DLCFundingInput(prevTx,
                                prevTxVout,
                                sequence,
                                maxWitnessLen,
                                redeemScriptOpt)
              }
          }
          .get
          .toVector

      val oracleInfo =
        vec.find(_._1 == "oracleInfo").map(obj => OracleInfo(obj._2.str)).get

      val pubKeys =
        vec
          .find(_._1 == "pubKeys")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val fundingKey = getValue("fundingKey")
              val payoutAddress = getValue("payoutAddress")

              DLCPublicKeys(
                ECPublicKey(fundingKey.str),
                BitcoinAddress(payoutAddress.str)
              )
          }
          .get

      val totalCollateral = vec
        .find(_._1 == "totalCollateral")
        .map(obj => Satoshis(obj._2.num.toLong))
        .get
      val changeAddress =
        vec
          .find(_._1 == "changeAddress")
          .map(obj => BitcoinAddress.fromString(obj._2.str))
          .get
      val feeRate =
        vec
          .find(_._1 == "feeRate")
          .map(obj => SatoshisPerVirtualByte(Satoshis(obj._2.num.toLong)))
          .get

      val timeouts =
        vec
          .find(_._1 == "timeouts")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj
              val contractMaturity = getValue("contractMaturity")
              val contractTimeout = getValue("contractTimeout")

              DLCTimeouts(
                BlockTime(UInt32(contractMaturity.num.toLong)),
                BlockTime(UInt32(contractTimeout.num.toLong))
              )
          }
          .get

      val oracleAndContractInfo =
        OracleAndContractInfo(oracleInfo, contractInfo)

      DLCOffer(oracleAndContractInfo.contractInfo,
               oracleAndContractInfo.oracleInfo,
               pubKeys,
               totalCollateral,
               fundingInputs,
               changeAddress,
               feeRate,
               timeouts)

    }
  }

  case class DLCAcceptWithoutSigs(
      totalCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      tempContractId: Sha256Digest) {

    def withSigs(cetSigs: CETSignatures[_]): DLCAccept = {
      DLCAccept(totalCollateral = totalCollateral,
                pubKeys = pubKeys,
                fundingInputs = fundingInputs,
                changeAddress = changeAddress,
                cetSigs = cetSigs,
                tempContractId = tempContractId)
    }
  }

  case class DLCAccept(
      totalCollateral: Satoshis,
      pubKeys: DLCPublicKeys,
      fundingInputs: Vector[DLCFundingInput],
      changeAddress: BitcoinAddress,
      cetSigs: CETSignatures[_],
      tempContractId: Sha256Digest)
      extends DLCSetupMessage {

    def toTLV: DLCAcceptTLV = {
      DLCAcceptTLV(
        tempContractId = tempContractId,
        totalCollateralSatoshis = totalCollateral,
        fundingPubKey = pubKeys.fundingKey,
        payoutSPK = pubKeys.payoutAddress.scriptPubKey,
        fundingInputs = fundingInputs.map(_.toTLV),
        changeSPK = changeAddress.scriptPubKey,
        cetSignatures = CETSignaturesV0TLV(cetSigs.outcomeSigs.values.toVector),
        refundSignature = cetSigs.refundSig.signature
      )
    }

    def toMessage: LnMessage[DLCAcceptTLV] = {
      LnMessage(this.toTLV)
    }

    def toJson: Value = {
      val fundingInputsJson =
        fundingInputs.map { input =>
          val obj = mutable.LinkedHashMap(
            "prevTx" -> Str(input.prevTx.hex),
            "prevTxVout" -> Num(input.prevTxVout.toInt),
            "sequence" -> Num(input.sequence.toInt),
            "maxWitnessLength" -> Num(input.maxWitnessLen.toInt)
          )

          input.redeemScriptOpt.foreach { redeemScript =>
            obj.+=("redeemScript" -> Str(redeemScript.hex))
          }

          obj
        }

      val outcomeSigsJson =
        cetSigs.asInstanceOf[CETSignatures[Sha256Digest]].outcomeSigs.map {
          case (hash, sig) =>
            mutable.LinkedHashMap(hash.hex -> Str(sig.hex))
        }

      val cetSigsJson =
        mutable.LinkedHashMap("outcomeSigs" -> Value(outcomeSigsJson),
                              "refundSig" -> Str(cetSigs.refundSig.hex))
      val pubKeysJson =
        mutable.LinkedHashMap(
          "fundingKey" -> Str(pubKeys.fundingKey.hex),
          "payoutAddress" -> Str(pubKeys.payoutAddress.value)
        )

      Obj(
        mutable.LinkedHashMap[String, Value](
          "totalCollateral" -> Num(totalCollateral.toLong.toDouble),
          "pubKeys" -> pubKeysJson,
          "fundingInputs" -> fundingInputsJson,
          "changeAddress" -> Str(changeAddress.value),
          "cetSigs" -> cetSigsJson,
          "tempContractId" -> Str(tempContractId.hex)
        )
      )
    }

    def withoutSigs: DLCAcceptWithoutSigs = {
      DLCAcceptWithoutSigs(totalCollateral,
                           pubKeys,
                           fundingInputs,
                           changeAddress,
                           tempContractId)
    }
  }

  object DLCAccept {

    def fromTLV(
        accept: DLCAcceptTLV,
        network: NetworkParameters,
        outcomes: Vector[Sha256Digest]): DLCAccept = {
      val outcomeSigs = accept.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          outcomes.zip(sigs).toMap
      }

      DLCAccept(
        totalCollateral = accept.totalCollateralSatoshis,
        pubKeys = DLCPublicKeys(
          accept.fundingPubKey,
          BitcoinAddress.fromScriptPubKey(accept.payoutSPK, network)),
        fundingInputs = accept.fundingInputs.map {
          case input: FundingInputV0TLV => DLCFundingInput.fromTLV(input)
        },
        changeAddress =
          BitcoinAddress.fromScriptPubKey(accept.changeSPK, network),
        cetSigs = CETSignatures(
          outcomeSigs,
          PartialSignature(accept.fundingPubKey, accept.refundSignature)),
        tempContractId = accept.tempContractId
      )
    }

    def fromTLV(accept: DLCAcceptTLV, offer: DLCOffer[_]): DLCAccept = {
      offer.contractInfo match {
        case SingleNonceContractInfo(outcomeValueMap) =>
          fromTLV(accept,
                  offer.changeAddress.networkParameters,
                  outcomeValueMap.keys.toVector)
        case MultiNonceContractInfo(_) => ???
      }
    }

    def fromMessage(
        accept: LnMessage[DLCAcceptTLV],
        offer: DLCOffer[_]): DLCAccept = {
      fromTLV(accept.tlv, offer)
    }

    def fromJson(js: Value): DLCAccept = {
      val vec = js.obj.toVector

      val totalCollateral = vec
        .find(_._1 == "totalCollateral")
        .map(obj => Satoshis(obj._2.num.toLong))
        .get
      val changeAddress =
        vec
          .find(_._1 == "changeAddress")
          .map(obj => BitcoinAddress.fromString(obj._2.str))
          .get

      val pubKeys =
        vec
          .find(_._1 == "pubKeys")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val fundingKey =
                getValue("fundingKey")
              val payoutAddress =
                getValue("payoutAddress")

              DLCPublicKeys(
                ECPublicKey(fundingKey.str),
                BitcoinAddress(payoutAddress.str)
              )
          }
          .get

      val fundingInputs =
        vec
          .find(_._1 == "fundingInputs")
          .map {
            case (_, value) =>
              value.arr.map { subVal =>
                implicit val obj: mutable.LinkedHashMap[String, Value] =
                  subVal.obj

                val prevTx = Transaction(getValue("prevTx").str)
                val prevTxVout = UInt32(getValue("prevTxVout").num.toInt)
                val sequence = UInt32(getValue("sequence").num.toLong)
                val maxWitnessLen =
                  UInt16(getValue("maxWitnessLength").num.toInt)
                val redeemScriptOpt = obj.find(_._1 == "redeemScript").map {
                  case (_, redeemScript) =>
                    WitnessScriptPubKey(redeemScript.str)
                }

                DLCFundingInput(prevTx,
                                prevTxVout,
                                sequence,
                                maxWitnessLen,
                                redeemScriptOpt)
              }
          }
          .get
          .toVector

      val cetSigs =
        vec
          .find(_._1 == "cetSigs")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val outcomeSigsMap = getValue("outcomeSigs")
              val outcomeSigs = outcomeSigsMap.arr.map { v =>
                val (key, value) = v.obj.head
                val hash = Sha256Digest(key)
                val sig = ECAdaptorSignature(value.str)
                (hash, sig)
              }

              val refundSig = getValue("refundSig")

              CETSignatures(
                outcomeSigs.toMap,
                PartialSignature(refundSig.str)
              )
          }
          .get

      val tempContractId =
        vec
          .find(_._1 == "tempContractId")
          .map(obj => Sha256Digest(obj._2.str))
          .get

      DLCAccept(totalCollateral,
                pubKeys,
                fundingInputs,
                changeAddress,
                cetSigs,
                tempContractId)
    }
  }

  case class DLCSign(
      cetSigs: CETSignatures[_],
      fundingSigs: FundingSignatures,
      contractId: ByteVector)
      extends DLCMessage {

    def toTLV: DLCSignTLV = {
      DLCSignTLV(
        contractId = contractId,
        cetSignatures = CETSignaturesV0TLV(cetSigs.outcomeSigs.values.toVector),
        refundSignature = ECDigitalSignature.fromFrontOfBytes(
          cetSigs.refundSig.signature.bytes),
        fundingSignatures = fundingSigs.toTLV
      )
    }

    def toMessage: LnMessage[DLCSignTLV] = {
      LnMessage(this.toTLV)
    }

    def toJson: Value = {

      val fundingSigsMap = fundingSigs.map {
        case (outPoint, scriptWitness) =>
          (outPoint.hex, Str(scriptWitness.hex))
      }

      val fundingSigsJson = fundingSigsMap
        .foldLeft(mutable.LinkedHashMap.newBuilder[String, Value])(
          (builder, element) => builder += element)
        .result()

      val outcomeSigsJson =
        cetSigs.asInstanceOf[CETSignatures[Sha256Digest]].outcomeSigs.map {
          case (hash, sig) =>
            mutable.LinkedHashMap(hash.hex -> Str(sig.hex))
        }

      val cetSigsJson =
        mutable.LinkedHashMap("outcomeSigs" -> Value(outcomeSigsJson),
                              "refundSig" -> Str(cetSigs.refundSig.hex))

      Obj(
        mutable.LinkedHashMap[String, Value](
          "cetSigs" -> cetSigsJson,
          "fundingSigs" -> fundingSigsJson,
          "contractId" -> Str(contractId.toHex)
        )
      )
    }
  }

  object DLCSign {

    def fromTLV(
        sign: DLCSignTLV,
        fundingPubKey: ECPublicKey,
        outcomes: Vector[Sha256Digest],
        fundingOutPoints: Vector[TransactionOutPoint]): DLCSign = {
      val outcomeSigs = sign.cetSignatures match {
        case CETSignaturesV0TLV(sigs) =>
          outcomes.zip(sigs).toMap
      }

      val sigs = sign.fundingSignatures match {
        case FundingSignaturesV0TLV(witnesses) => witnesses
      }

      val fundingSigs = fundingOutPoints.zip(sigs)

      DLCSign(
        cetSigs = CETSignatures(
          outcomeSigs,
          PartialSignature(
            fundingPubKey,
            ECDigitalSignature(
              sign.refundSignature.bytes :+ HashType.sigHashAll.byte))),
        fundingSigs = FundingSignatures(fundingSigs),
        contractId = sign.contractId
      )
    }

    def fromTLV(sign: DLCSignTLV, offer: DLCOffer[_]): DLCSign = {
      offer.contractInfo match {
        case SingleNonceContractInfo(outcomeValueMap) =>
          fromTLV(sign,
                  offer.pubKeys.fundingKey,
                  outcomeValueMap.keys.toVector,
                  offer.fundingInputs.map(_.outPoint))
        case MultiNonceContractInfo(_) => ???
      }
    }

    def fromMessage(
        sign: LnMessage[DLCSignTLV],
        offer: DLCOffer[_]): DLCSign = {
      fromTLV(sign.tlv, offer)
    }

    def fromJson(js: Value): DLCSign = {
      val vec = js.obj.toVector

      val cetSigs =
        vec
          .find(_._1 == "cetSigs")
          .map {
            case (_, value) =>
              implicit val obj: mutable.LinkedHashMap[String, Value] = value.obj

              val outcomeSigsMap = getValue("outcomeSigs")
              val outcomeSigs = outcomeSigsMap.arr.map { item =>
                val (key, value) = item.obj.head
                val hash = Sha256Digest(key)
                val sig = ECAdaptorSignature(value.str)
                (hash, sig)
              }

              val refundSig = getValue("refundSig")

              CETSignatures(
                outcomeSigs.toMap,
                PartialSignature(refundSig.str)
              )
          }
          .get

      val fundingSigs =
        vec
          .find(_._1 == "fundingSigs")
          .map {
            case (_, value) =>
              if (value.obj.isEmpty) {
                throw new RuntimeException(
                  s"DLC Sign cannot have empty fundingSigs, got $js")
              } else {
                value.obj.toVector.map {
                  case (outPoint, scriptWitness) =>
                    (TransactionOutPoint(outPoint),
                     RawScriptWitnessParser
                       .read(scriptWitness.str)
                       .asInstanceOf[ScriptWitnessV0])
                }
              }
          }
          .get

      val contractId =
        vec
          .find(_._1 == "contractId")
          .map(obj => ByteVector.fromValidHex(obj._2.str))
          .get

      DLCSign(cetSigs, FundingSignatures(fundingSigs), contractId)
    }
  }
}
