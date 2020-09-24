package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCOffer,
  OracleInfo
}
import org.bitcoins.commons.jsonmodels.dlc.{
  DLCFundingInput,
  DLCFundingInputP2WPKHV0,
  DLCPublicKeys,
  DLCTimeouts
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.tlv.{
  DLCAcceptTLV,
  DLCOfferTLV,
  DLCSignTLV,
  LnMessage,
  LnMessageFactory
}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionConstants,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{P2WPKHV0InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import play.api.libs.json._

sealed trait DLCTestVector {
  def toJson: JsValue
}

object DLCTestVector {

  def fromJson(json: JsValue): JsResult[DLCTestVector] = {
    SuccessTestVector.fromJson(json)
  }
}

case class FundingInputTx(tx: Transaction, idx: Int, inputKey: ECPrivateKey) {

  val outputRef: OutputReference =
    OutputReference(TransactionOutPoint(tx.txId, UInt32(idx)), tx.outputs(idx))

  def toFundingInput: DLCFundingInput =
    DLCFundingInputP2WPKHV0(tx, UInt32(idx), TransactionConstants.sequence)
}

// Currently only supports P2WPKH inputs
case class DLCPartyParams(
    collateral: CurrencyUnit,
    fundingInputTxs: Vector[FundingInputTx],
    changeAddress: BitcoinAddress,
    fundingPrivKey: ECPrivateKey,
    payoutAddress: BitcoinAddress) {

  lazy val fundingInputs: Vector[DLCFundingInput] =
    fundingInputTxs.map(_.toFundingInput)

  lazy val fundingScriptSigParams: Vector[
    ScriptSignatureParams[P2WPKHV0InputInfo]] = {
    fundingInputTxs
      .map { fundingInputTx =>
        val OutputReference(outPoint, output) = fundingInputTx.outputRef
        ScriptSignatureParams(
          P2WPKHV0InputInfo(outPoint,
                            output.value,
                            fundingInputTx.inputKey.publicKey),
          fundingInputTx.tx,
          fundingInputTx.inputKey,
          HashType.sigHashAll
        )
      }
  }

  def toOffer(params: DLCParams): DLCOffer = {
    DLCOffer(
      params.contractInfo,
      params.oracleInfo,
      DLCPublicKeys(fundingPrivKey.publicKey, payoutAddress),
      collateral.satoshis,
      fundingInputs,
      changeAddress,
      params.feeRate,
      DLCTimeouts(params.contractMaturityBound, params.contractTimeout)
    )
  }
}

case class DLCParams(
    oracleInfo: OracleInfo,
    contractInfo: ContractInfo,
    contractMaturityBound: BlockTimeStamp,
    contractTimeout: BlockTimeStamp,
    feeRate: SatoshisPerVirtualByte,
    realOutcome: Sha256Digest,
    oracleSignature: SchnorrDigitalSignature)

case class ValidTestInputs(
    params: DLCParams,
    offerParams: DLCPartyParams,
    acceptParams: DLCPartyParams) {
  lazy val calcOffer: DLCOffer = offerParams.toOffer(params)
}

object ValidTestInputs {

  def fromJson(json: JsValue): JsResult[ValidTestInputs] = {
    Json.fromJson(json)(SuccessTestVector.validTestInputsFormat)
  }
}

case class DLCTransactions(
    fundingTx: Transaction,
    cets: Vector[Transaction],
    refundTx: Transaction)

case class SuccessTestVector(
    testInputs: ValidTestInputs,
    offer: LnMessage[DLCOfferTLV],
    accept: LnMessage[DLCAcceptTLV],
    sign: LnMessage[DLCSignTLV],
    unsignedTxs: DLCTransactions,
    signedTxs: DLCTransactions)
    extends DLCTestVector {

  override def toJson: JsValue = {
    Json.toJson(this)(SuccessTestVector.successTestVectorFormat)
  }

  def toJsonStr: String = {
    Json.prettyPrint(toJson)
  }
}

object SuccessTestVector {

  def hexFormat[T <: NetworkElement](factory: Factory[T]): Format[T] =
    Format[T](
      { hex => hex.validate[String].map(factory.fromHex) },
      { element => JsString(element.hex) }
    )

  implicit val oracleInfoFormat: Format[OracleInfo] = Format[OracleInfo](
    {
      _.validate[Map[String, String]]
        .map(map =>
          OracleInfo(SchnorrPublicKey(map("publicKey")),
                     SchnorrNonce(map("nonce"))))
    },
    { info =>
      Json.toJson(
        Map("publicKey" -> info.pubKey.hex, "nonce" -> info.rValue.hex))
    }
  )

  implicit val contractInfoFormat: Format[ContractInfo] =
    Format[ContractInfo](
      {
        _.validate[Vector[Map[String, Long]]]
          .map(_.map { outcomeToAmt =>
            val (outcome, amt) = outcomeToAmt.head
            Sha256Digest(outcome) -> Satoshis(amt)
          }.toMap)
          .map(ContractInfo.apply)
      },
      { info =>
        Json.toJson(info.outcomeValueMap.toVector.map {
          case (outcome, amt) =>
            JsObject(Map(outcome.hex -> JsNumber(amt.toLong)))
        })
      }
    )

  implicit val blockTimeStampFormat: Format[BlockTimeStamp] =
    Format[BlockTimeStamp](
      { _.validate[Long].map(UInt32.apply).map(BlockTimeStamp.apply) },
      { stamp => JsNumber(stamp.toUInt32.toLong) }
    )

  implicit val satsPerVBFormat: Format[SatoshisPerVirtualByte] =
    Format[SatoshisPerVirtualByte](
      {
        _.validate[Long].map(Satoshis.apply).map(SatoshisPerVirtualByte.apply)
      },
      { satsPerVB => JsNumber(satsPerVB.toLong) }
    )

  implicit val sha256DigestFormat: Format[Sha256Digest] = hexFormat(
    Sha256Digest)

  implicit val schnorrDigitalSignatureFormat: Format[SchnorrDigitalSignature] =
    hexFormat(SchnorrDigitalSignature)

  implicit val currencyUnitFormat: Format[CurrencyUnit] =
    Format[CurrencyUnit](
      { _.validate[Long].map(Satoshis.apply) },
      { currency => JsNumber(currency.satoshis.toLong) }
    )
  implicit val transactionFormat: Format[Transaction] = hexFormat(Transaction)
  implicit val ecPrivKeyFormat: Format[ECPrivateKey] = hexFormat(ECPrivateKey)

  implicit val fundingInputTxFormat: Format[FundingInputTx] =
    Json.format[FundingInputTx]

  implicit val addressFormat: Format[BitcoinAddress] =
    Format[BitcoinAddress](
      { _.validate[String].map(BitcoinAddress.fromString) },
      { address => JsString(address.toString) }
    )
  implicit val dlcParamFormat: Format[DLCParams] = Json.format[DLCParams]

  implicit val DLCPartyParamsFormat: Format[DLCPartyParams] =
    Json.format[DLCPartyParams]

  implicit val offerMsgFormat: Format[LnMessage[DLCOfferTLV]] = hexFormat(
    LnMessageFactory(DLCOfferTLV))

  implicit val acceptMsgFormat: Format[LnMessage[DLCAcceptTLV]] = hexFormat(
    LnMessageFactory(DLCAcceptTLV))

  implicit val signMsgFormat: Format[LnMessage[DLCSignTLV]] = hexFormat(
    LnMessageFactory(DLCSignTLV))

  implicit val validTestInputsFormat: Format[ValidTestInputs] =
    Json.format[ValidTestInputs]

  implicit val dlcTransactionsFormat: Format[DLCTransactions] =
    Json.format[DLCTransactions]

  implicit val successTestVectorFormat: Format[SuccessTestVector] =
    Json.format[SuccessTestVector]

  def fromJson(json: JsValue): JsResult[SuccessTestVector] = {
    json.validate[SuccessTestVector]
  }

  def fromString(str: String): JsResult[SuccessTestVector] = {
    fromJson(Json.parse(str))
  }
}
