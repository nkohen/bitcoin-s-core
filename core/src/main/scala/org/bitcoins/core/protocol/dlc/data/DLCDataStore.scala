package org.bitcoins.core.protocol.dlc.data

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

  def writeOffer(dlcOffer: DLCOffer): Unit = {
    val DLCOffer(contractInfo,
                 DLCPublicKeys(fundingKey, finalAddress),
                 collateral,
                 fundingInputs,
                 changeAddress,
                 payoutSerialId,
                 changeSerialId,
                 fundOutputSerialId,
                 feeRate,
                 timeouts) = dlcOffer

    global.setContractInfo(contractInfo)
    offer.setFundingKey(fundingKey)
    offer.setFinalAddress(finalAddress)
    offer.setCollateral(collateral)
    offer.setFundingInputs(fundingInputs)
    offer.setChangeAddress(changeAddress)
    offer.setPayoutSerialId(payoutSerialId)
    offer.setChangeSerialId(changeSerialId)
    global.setFundOutputSerialId(fundOutputSerialId)
    global.setFeeRate(feeRate)
    global.setTimeouts(timeouts)
    global.setTempContractId(dlcOffer.tempContractId)
  }

  def offerOpt: Option[DLCOffer] = {
    for {
      contractInfo <- global.contractInfoOpt
      fundingKey <- offer.fundingKeyOpt
      finalAddress <- offer.finalAddressOpt
      collateral <- offer.collateralOpt
      fundingInputs <- offer.fundingInputsOpt
      changeAddress <- offer.changeAddressOpt
      payoutSerialId <- offer.payoutSerialIdOpt
      changeSerialId <- offer.changeSerialIdOpt
      fundOutputSerialId <- global.fundOutputSerialIdOpt
      feeRate <- global.feeRateOpt
      timeouts <- global.timeoutsOpt
    } yield {
      DLCOffer(
        contractInfo,
        DLCPublicKeys(fundingKey, finalAddress),
        collateral,
        fundingInputs,
        changeAddress,
        payoutSerialId,
        changeSerialId,
        fundOutputSerialId,
        feeRate,
        timeouts
      )
    }
  }

  def writeAcceptWithoutSigs(dlcAccept: DLCAcceptWithoutSigs): Unit = {
    val DLCAcceptWithoutSigs(collateral,
                             DLCPublicKeys(fundingKey, finalAddress),
                             fundingInputs,
                             changeAddress,
                             payoutSerialId,
                             changeSerialId,
                             negotiationFields,
                             tempContractId) = dlcAccept

    accept.setCollateral(collateral)
    accept.setFundingKey(fundingKey)
    accept.setFinalAddress(finalAddress)
    accept.setFundingInputs(fundingInputs)
    accept.setChangeAddress(changeAddress)
    accept.setPayoutSerialId(payoutSerialId)
    accept.setChangeSerialId(changeSerialId)
    accept.setNegotiationFields(negotiationFields)
    global.setTempContractId(tempContractId) // TODO: Validate
  }

  def writeAccept(dlcAccept: DLCAccept): Unit = {
    writeAcceptWithoutSigs(dlcAccept.withoutSigs)
    val sigStore = if (local.getter.isInitiator) remote else local
    sigStore.setCetSigs(dlcAccept.cetSigs)
  }

  def acceptWithoutSigsOpt: Option[DLCAcceptWithoutSigs] = {
    for {
      collateral <- accept.collateralOpt
      fundingKey <- accept.fundingKeyOpt
      finalAddress <- accept.finalAddressOpt
      fundingInputs <- accept.fundingInputsOpt
      changeAddress <- accept.changeAddressOpt
      payoutSerialId <- accept.payoutSerialIdOpt
      changeSerialId <- accept.changeSerialIdOpt
      negotiationFields <- accept.negotiationFieldsOpt
      tempContractId <- global.tempContractIdOpt
    } yield {
      DLCAcceptWithoutSigs(collateral,
                           DLCPublicKeys(fundingKey, finalAddress),
                           fundingInputs,
                           changeAddress,
                           payoutSerialId,
                           changeSerialId,
                           negotiationFields,
                           tempContractId)
    }
  }

  def acceptOpt: Option[DLCAccept] = {
    for {
      acceptWithoutSigs <- acceptWithoutSigsOpt
      contractInfo <- global.contractInfoOpt
      isInitiator <- local.isInitiatorOpt
      acceptStore = if (isInitiator) remote else local
      cetSigs <- acceptStore.cetSigsOpt(contractInfo)
    } yield {
      acceptWithoutSigs.withSigs(cetSigs)
    }
  }

  def writeSign(dlcSign: DLCSign): Unit = {
    val DLCSign(cetSigs, fundingSigs, contractId) = dlcSign

    val sigStore = if (local.getter.isInitiator) local else remote
    sigStore.setCetSigs(cetSigs)
    sigStore.setFundingSigs(fundingSigs)
    global.setContractId(contractId) // TODO: Validate
  }

  def signOpt: Option[DLCSign] = {
    for {
      contractId <- global.contractIdOpt
      contractInfo <- global.contractInfoOpt
      isInitiator <- local.isInitiatorOpt
      offerStore = if (isInitiator) local else remote
      fundingSigs <- offerStore.fundingSigsOpt
      cetSigs <- offerStore.cetSigsOpt(contractInfo)
    } yield {
      DLCSign(cetSigs, fundingSigs, contractId)
    }
  }
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

  def setContractInfo(contractInfo: ContractInfo): Unit
  def setFundOutputSerialId(fundOutputSerialId: UInt64): Unit
  def setFeeRate(feeRate: SatoshisPerVirtualByte): Unit
  def setCetTimeout(cetTimeout: BlockTimeStamp): Unit
  def setContractTimeout(contractTimeout: BlockTimeStamp): Unit
  def setTempContractId(tempContractId: Sha256Digest): Unit
  def setContractId(contractId: ByteVector): Unit
  def setState(state: DLCState): Unit
  def setFundingTx(fundingTx: WitnessTransaction): Unit
  def setClosingTx(closingTx: WitnessTransaction): Unit
  def setOracleSigs(oracleSigs: Vector[SchnorrDigitalSignature]): Unit
  def setOraclesUsed(oraclesUsed: Vector[SingleOracleInfo]): Unit

  def setTimeouts(timeouts: DLCTimeouts): Unit = {
    setCetTimeout(timeouts.contractMaturity)
    setContractTimeout(timeouts.contractTimeout)
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

  def setFundingKey(fundingKey: ECPublicKey): Unit
  def setFinalAddress(finalAddress: BitcoinAddress): Unit
  def setCollateral(collateral: Satoshis): Unit
  def setFundingInputs(fundingInputs: Vector[DLCFundingInput]): Unit
  def setChangeAddress(changeAddress: BitcoinAddress): Unit
  def setPayoutSerialId(payoutSerialId: UInt64): Unit
  def setChangeSerialId(changeSerialId: UInt64): Unit
}

trait DLCOfferPartyDataStore extends DLCPartyDataStore {

  override def getter: DLCOfferPartyDataStoreGetter =
    DLCOfferPartyDataStoreGetter(this)
}

trait DLCAcceptPartyDataStore extends DLCPartyDataStore {

  override def getter: DLCAcceptPartyDataStoreGetter =
    DLCAcceptPartyDataStoreGetter(this)

  def negotiationFieldsOpt: Option[DLCAccept.NegotiationFields]

  def setNegotiationFields(negotiationFields: DLCAccept.NegotiationFields): Unit
}

trait DLCSignatureDataStore extends DLCDataStore {
  override def getter: DLCSignatureDataStoreGetter

  def fundingSigsOpt: Option[FundingSignatures]
  def cetAdaptorSigsOpt: Option[Vector[(ECPublicKey, ECAdaptorSignature)]]
  def refundSigOpt: Option[PartialSignature]

  def cetSigsOpt(contractInfo: ContractInfo): Option[CETSignatures] = {
    for {
      cetAdaptorSigs <- cetAdaptorSigsOpt
      refundSig <- refundSigOpt
    } yield {
      val sigMap = immutable.HashMap.from(cetAdaptorSigs)
      val cetSigsWithOutcomes = contractInfo.allOutcomes.map { outcome =>
        outcome -> sigMap(outcome.sigPoint)
      }
      CETSignatures(cetSigsWithOutcomes, refundSig)
    }
  }

  def setFundingSigs(fundingSigs: FundingSignatures): Unit

  def setCetAdaptorSigs(
      cetAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)]): Unit
  def setRefundSig(refundSig: PartialSignature): Unit

  def setCetSigs(cetSigs: CETSignatures): Unit = {
    val cetAdaptorSigs = cetSigs.outcomeSigs.map { case (outcome, sig) =>
      (outcome.sigPoint, sig)
    }
    setCetAdaptorSigs(cetAdaptorSigs)
    setRefundSig(cetSigs.refundSig)
  }
}

trait DLCLocalDataStore extends DLCSignatureDataStore {
  override def getter: DLCLocalDataStoreGetter = DLCLocalDataStoreGetter(this)

  def isInitiatorOpt: Option[Boolean]
  def fundingPrivKeyOpt: Option[AdaptorSign]
  def fundingUtxosOpt: Option[Vector[ScriptSignatureParams[InputInfo]]]

  def setIsInitiator(isInitiator: Boolean): Unit
  def setFundingPrivKey(fundingPrivKey: AdaptorSign): Unit

  def setFundingUtxos(
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]): Unit
}

trait DLCRemoteDataStore extends DLCSignatureDataStore {
  override def getter: DLCRemoteDataStoreGetter = DLCRemoteDataStoreGetter(this)
}
