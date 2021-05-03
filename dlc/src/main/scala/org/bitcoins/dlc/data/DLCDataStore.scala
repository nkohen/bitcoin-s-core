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

sealed trait DLCDataStore {
  def getter: DLCDataStoreGetter[DLCDataStore]
}

trait DLCFullDataStore extends DLCDataStore {

  override def getter: DLCFullDataStoreGetter = DLCFullDataStoreGetter(this)

  def global: DLCGlobalDataStore
  def local: DLCLocalDataStore
  def remote: DLCRemoteDataStore
  def offer: DLCOfferPartyDataStore
  def accept: DLCAcceptPartyDataStore
}

trait DLCGlobalDataStore extends DLCDataStore {
  override def getter: DLCGlobalDataStoreGetter = DLCGlobalDataStoreGetter(this)

  def contractInfoOpt: Option[ContractInfo]
  def fundOutputSerialIdOpt: Option[UInt64]
  def feeRateOpt: Option[SatoshisPerVirtualByte]
  def cetTimeoutOpt: Option[BlockTimeStamp]
  def contractTimeoutOpt: Option[BlockTimeStamp]
  def tempContractIdOpt: Option[Sha256Digest]
  def contractIdOpt: Option[ByteVector]
  def stateOpt: Option[DLCState]
  def fundingTxOpt: Option[WitnessTransaction]
  def closingTxOpt: Option[WitnessTransaction]
  def oracleSigsOpt: Option[Vector[SchnorrDigitalSignature]]
  def oraclesUsedOpt: Option[Vector[SingleOracleInfo]]

  def timeoutsOpt: Option[DLCTimeouts] = {
    for {
      cetTimeout <- cetTimeoutOpt
      contractTimeout <- contractTimeoutOpt
    } yield DLCTimeouts(cetTimeout, contractTimeout)
  }
}

trait DLCPartyDataStore extends DLCDataStore {
  override def getter: DLCPartyDataStoreGetter

  def fundingKeyOpt: Option[ECPublicKey]
  def finalAddressOpt: Option[BitcoinAddress]
  def collateralOpt: Option[Satoshis]
  def fundingInputsOpt: Option[Vector[DLCFundingInput]]
  def changeAddressOpt: Option[BitcoinAddress]
  def payoutSerialIdOpt: Option[UInt64]
  def changeSerialIdOpt: Option[UInt64]

  def finalSPKOpt: Option[ScriptPubKey] = {
    finalAddressOpt.map(_.scriptPubKey)
  }

  def changeSPKOpt: Option[ScriptPubKey] = {
    changeAddressOpt.map(_.scriptPubKey)
  }
}

trait DLCOfferPartyDataStore extends DLCPartyDataStore {

  override def getter: DLCOfferPartyDataStoreGetter =
    DLCOfferPartyDataStoreGetter(this)
}

trait DLCAcceptPartyDataStore extends DLCPartyDataStore {

  override def getter: DLCAcceptPartyDataStoreGetter =
    DLCAcceptPartyDataStoreGetter(this)

  def acceptNegotiationFieldsOpt: Option[DLCAccept.NegotiationFields]
}

trait DLCSignatureDataStore extends DLCDataStore {
  override def getter: DLCSignatureDataStoreGetter

  def fundingSigsOpt: Option[FundingSignatures]
  def cetSigsOpt: Option[Vector[(ECPublicKey, ECAdaptorSignature)]]
  def refundSigOpt: Option[PartialSignature]
}

trait DLCLocalDataStore extends DLCSignatureDataStore {
  override def getter: DLCLocalDataStoreGetter = DLCLocalDataStoreGetter(this)

  def isInitiatorOpt: Option[Boolean]
  def fundingPrivKeyOpt: Option[AdaptorSign]
  def fundingUtxosOpt: Option[Vector[ScriptSignatureParams[InputInfo]]]
}

trait DLCRemoteDataStore extends DLCSignatureDataStore {
  override def getter: DLCRemoteDataStoreGetter = DLCRemoteDataStoreGetter(this)
}
