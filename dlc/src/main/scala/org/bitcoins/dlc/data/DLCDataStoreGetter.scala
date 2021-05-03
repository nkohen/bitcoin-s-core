package org.bitcoins.dlc.data

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.DLCMessage.DLCAccept
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

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

  def acceptNegotiationFields: DLCAccept.NegotiationFields =
    get(_.acceptNegotiationFieldsOpt, "acceptNegotiationFields")
}

sealed trait DLCSignatureDataStoreGetter
    extends DLCDataStoreGetter[DLCSignatureDataStore] {
  def fundingSigs: FundingSignatures = get(_.fundingSigsOpt, "fundingSigs")

  def cetSigs: Vector[(ECPublicKey, ECAdaptorSignature)] =
    get(_.cetSigsOpt, "cetSigs")
  def refundSig: PartialSignature = get(_.refundSigOpt, "refundSig")
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
