package org.bitcoins.dlc.data

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

import scala.collection.immutable

sealed trait DLCDataStoreGetter[+Store <: DLCDataStore] {
  def store: Store

  protected def get[T](func: Store => Option[T], name: String): T = {
    func(store).getOrElse(
      throw new RuntimeException(s"Could not find $name in $store"))
  }
}

case class DLCFullDataStoreGetter(override val store: DLCFullDataStore)
    extends DLCDataStoreGetter[DLCFullDataStore] {
  def global: DLCGlobalDataStoreGetter = store.global.getter
  def local: DLCLocalDataStoreGetter = store.local.getter
  def remote: DLCRemoteDataStoreGetter = store.remote.getter
  def offer: DLCOfferPartyDataStoreGetter = store.offer.getter
  def accept: DLCAcceptPartyDataStoreGetter = store.accept.getter

  def getOffer: DLCOffer = {
    DLCOffer(
      global.contractInfo,
      DLCPublicKeys(offer.fundingKey, offer.finalAddress),
      offer.collateral,
      offer.fundingInputs,
      offer.changeAddress,
      offer.payoutSerialId,
      offer.changeSerialId,
      global.fundOutputSerialId,
      global.feeRate,
      global.timeouts
    )
  }

  def getAcceptWithoutSigs: DLCAcceptWithoutSigs = {
    DLCAcceptWithoutSigs(
      accept.collateral,
      DLCPublicKeys(accept.fundingKey, accept.finalAddress),
      accept.fundingInputs,
      accept.changeAddress,
      accept.payoutSerialId,
      accept.changeSerialId,
      accept.negotiationFields,
      global.tempContractId
    )
  }

  def getAccept: DLCAccept = {
    val acceptWithoutSigs = getAcceptWithoutSigs
    val contractInfo = global.contractInfo
    val isInitiator = local.isInitiator
    val acceptGetter = if (isInitiator) remote else local
    val cetSigs = acceptGetter.cetSigs(contractInfo)

    acceptWithoutSigs.withSigs(cetSigs)
  }

  def getSign: DLCSign = {
    val contractId = global.contractId
    val contractInfo = global.contractInfo
    val isInitiator = local.isInitiator
    val offerGetter = if (isInitiator) local else remote
    val fundingSigs = offerGetter.fundingSigs
    val cetSigs = offerGetter.cetSigs(contractInfo)

    DLCSign(cetSigs, fundingSigs, contractId)
  }
}

case class DLCGlobalDataStoreGetter(override val store: DLCGlobalDataStore)
    extends DLCDataStoreGetter[DLCGlobalDataStore] {
  def contractInfo: ContractInfo = get(_.contractInfoOpt, "contractInfo")

  def fundOutputSerialId: UInt64 =
    get(_.fundOutputSerialIdOpt, "fundOutputSerialId")
  def feeRate: SatoshisPerVirtualByte = get(_.feeRateOpt, "feeRate")
  def cetTimeout: BlockTimeStamp = get(_.cetTimeoutOpt, "cetTimeout")

  def contractTimeout: BlockTimeStamp =
    get(_.contractTimeoutOpt, "contractTimeout")
  def tempContractId: Sha256Digest = get(_.tempContractIdOpt, "tempContractId")
  def contractId: ByteVector = get(_.contractIdOpt, "contractId")
  def state: DLCState = get(_.stateOpt, "state")
  def fundingTx: WitnessTransaction = get(_.fundingTxOpt, "fundingTx")
  def closingTx: WitnessTransaction = get(_.closingTxOpt, "closingTx")

  def oracleSigs: Vector[SchnorrDigitalSignature] =
    get(_.oracleSigsOpt, "oracleSigs")

  def oraclesUsed: Vector[SingleOracleInfo] =
    get(_.oraclesUsedOpt, "oraclesUsed")

  def timeouts: DLCTimeouts = {
    DLCTimeouts(cetTimeout, contractTimeout)
  }
}

sealed trait DLCPartyDataStoreGetter
    extends DLCDataStoreGetter[DLCPartyDataStore] {
  def fundingKey: ECPublicKey = get(_.fundingKeyOpt, "fundingKey")
  def finalAddress: BitcoinAddress = get(_.finalAddressOpt, "finalAddress")
  def collateral: Satoshis = get(_.collateralOpt, "collateral")

  def fundingInputs: Vector[DLCFundingInput] =
    get(_.fundingInputsOpt, "fundingInputs")
  def changeAddress: BitcoinAddress = get(_.changeAddressOpt, "changeAddress")
  def payoutSerialId: UInt64 = get(_.payoutSerialIdOpt, "payoutSerialId")
  def changeSerialId: UInt64 = get(_.changeSerialIdOpt, "changeSerialId")

  def finalSPK: ScriptPubKey = {
    finalAddress.scriptPubKey
  }

  def changeSPK: ScriptPubKey = {
    changeAddress.scriptPubKey
  }
}

case class DLCOfferPartyDataStoreGetter(
    override val store: DLCOfferPartyDataStore)
    extends DLCPartyDataStoreGetter
    with DLCDataStoreGetter[DLCOfferPartyDataStore]

case class DLCAcceptPartyDataStoreGetter(
    override val store: DLCAcceptPartyDataStore)
    extends DLCPartyDataStoreGetter
    with DLCDataStoreGetter[DLCAcceptPartyDataStore] {

  def negotiationFields: DLCAccept.NegotiationFields =
    get(_.negotiationFieldsOpt, "negotiationFields")
}

sealed trait DLCSignatureDataStoreGetter
    extends DLCDataStoreGetter[DLCSignatureDataStore] {
  def fundingSigs: FundingSignatures = get(_.fundingSigsOpt, "fundingSigs")

  def cetAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)] =
    get(_.cetAdaptorSigsOpt, "cetAdaptorSigs")
  def refundSig: PartialSignature = get(_.refundSigOpt, "refundSig")

  def cetSigs(contractInfo: ContractInfo): CETSignatures = {
    val sigMap = immutable.HashMap.from(cetAdaptorSigs)
    val cetSigsWithOutcomes = contractInfo.allOutcomes.map { outcome =>
      outcome -> sigMap(outcome.sigPoint)
    }
    CETSignatures(cetSigsWithOutcomes, refundSig)
  }
}

case class DLCLocalDataStoreGetter(override val store: DLCLocalDataStore)
    extends DLCSignatureDataStoreGetter
    with DLCDataStoreGetter[DLCLocalDataStore] {
  def isInitiator: Boolean = get(_.isInitiatorOpt, "isInitiator")
  def fundingPrivKey: AdaptorSign = get(_.fundingPrivKeyOpt, "fundingPrivKey")

  def fundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
    get(_.fundingUtxosOpt, "fundingUtxos")
}

case class DLCRemoteDataStoreGetter(override val store: DLCRemoteDataStore)
    extends DLCSignatureDataStoreGetter
    with DLCDataStoreGetter[DLCRemoteDataStore]
