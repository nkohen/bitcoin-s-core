package org.bitcoins.dlc.data

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.DLCMessage.DLCAccept
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import scodec.bits.ByteVector

case class InMemoryDLCDataStore(
    override val global: InMemoryDLCGlobalDataStore =
      InMemoryDLCGlobalDataStore(),
    override val local: InMemoryDLCLocalDataStore = InMemoryDLCLocalDataStore(),
    override val remote: InMemoryDLCRemoteDataStore =
      InMemoryDLCRemoteDataStore(),
    override val offer: InMemoryDLCOfferPartyDataStore =
      InMemoryDLCOfferPartyDataStore(),
    override val accept: InMemoryDLCAcceptPartyDataStore =
      InMemoryDLCAcceptPartyDataStore())
    extends DLCFullDataStore

case class InMemoryDLCGlobalDataStore(
    var contractInfoOpt: Option[ContractInfo] = None,
    var fundOutputSerialIdOpt: Option[UInt64] = None,
    var feeRateOpt: Option[SatoshisPerVirtualByte] = None,
    var cetTimeoutOpt: Option[BlockTimeStamp] = None,
    var contractTimeoutOpt: Option[BlockTimeStamp] = None,
    var tempContractIdOpt: Option[Sha256Digest] = None,
    var contractIdOpt: Option[ByteVector] = None,
    var stateOpt: Option[DLCState] = None,
    var fundingTxOpt: Option[WitnessTransaction] = None,
    var closingTxOpt: Option[WitnessTransaction] = None,
    var oracleSigsOpt: Option[Vector[SchnorrDigitalSignature]] = None,
    var oraclesUsedOpt: Option[Vector[SingleOracleInfo]] = None)
    extends DLCGlobalDataStore {

  override def setContractInfo(contractInfo: ContractInfo): Unit =
    contractInfoOpt = Some(contractInfo)

  override def setFundOutputSerialId(fundOutputSerialId: UInt64): Unit =
    fundOutputSerialIdOpt = Some(fundOutputSerialId)

  override def setFeeRate(feeRate: SatoshisPerVirtualByte): Unit =
    feeRateOpt = Some(feeRate)

  override def setCetTimeout(cetTimeout: BlockTimeStamp): Unit =
    cetTimeoutOpt = Some(cetTimeout)

  override def setContractTimeout(contractTimeout: BlockTimeStamp): Unit =
    contractTimeoutOpt = Some(contractTimeout)

  override def setTempContractId(tempContractId: Sha256Digest): Unit =
    tempContractIdOpt = Some(tempContractId)

  override def setContractId(contractId: ByteVector): Unit =
    contractIdOpt = Some(contractId)

  override def setState(state: DLCState): Unit =
    stateOpt = Some(state)

  override def setFundingTx(fundingTx: WitnessTransaction): Unit =
    fundingTxOpt = Some(fundingTx)

  override def setClosingTx(closingTx: WitnessTransaction): Unit =
    closingTxOpt = Some(closingTx)

  override def setOracleSigs(
      oracleSigs: Vector[SchnorrDigitalSignature]): Unit =
    oracleSigsOpt = Some(oracleSigs)

  override def setOraclesUsed(oraclesUsed: Vector[SingleOracleInfo]): Unit =
    oraclesUsedOpt = Some(oraclesUsed)
}

case class InMemoryDLCOfferPartyDataStore(
    var fundingKeyOpt: Option[ECPublicKey] = None,
    var finalAddressOpt: Option[BitcoinAddress] = None,
    var collateralOpt: Option[Satoshis] = None,
    var fundingInputsOpt: Option[Vector[DLCFundingInput]] = None,
    var changeAddressOpt: Option[BitcoinAddress] = None,
    var payoutSerialIdOpt: Option[UInt64] = None,
    var changeSerialIdOpt: Option[UInt64] = None)
    extends DLCOfferPartyDataStore {

  override def setFundingKey(fundingKey: ECPublicKey): Unit =
    fundingKeyOpt = Some(fundingKey)

  override def setFinalAddress(finalAddress: BitcoinAddress): Unit =
    finalAddressOpt = Some(finalAddress)

  override def setCollateral(collateral: Satoshis): Unit =
    collateralOpt = Some(collateral)

  override def setFundingInputs(fundingInputs: Vector[DLCFundingInput]): Unit =
    fundingInputsOpt = Some(fundingInputs)

  override def setChangeAddress(changeAddress: BitcoinAddress): Unit =
    changeAddressOpt = Some(changeAddress)

  override def setPayoutSerialId(payoutSerialId: UInt64): Unit =
    payoutSerialIdOpt = Some(payoutSerialId)

  override def setChangeSerialId(changeSerialId: UInt64): Unit =
    changeSerialIdOpt = Some(changeSerialId)
}

case class InMemoryDLCAcceptPartyDataStore(
    var fundingKeyOpt: Option[ECPublicKey] = None,
    var finalAddressOpt: Option[BitcoinAddress] = None,
    var collateralOpt: Option[Satoshis] = None,
    var fundingInputsOpt: Option[Vector[DLCFundingInput]] = None,
    var changeAddressOpt: Option[BitcoinAddress] = None,
    var payoutSerialIdOpt: Option[UInt64] = None,
    var changeSerialIdOpt: Option[UInt64] = None,
    var negotiationFieldsOpt: Option[DLCAccept.NegotiationFields] = None)
    extends DLCAcceptPartyDataStore {

  override def setFundingKey(fundingKey: ECPublicKey): Unit =
    fundingKeyOpt = Some(fundingKey)

  override def setFinalAddress(finalAddress: BitcoinAddress): Unit =
    finalAddressOpt = Some(finalAddress)

  override def setCollateral(collateral: Satoshis): Unit =
    collateralOpt = Some(collateral)

  override def setFundingInputs(fundingInputs: Vector[DLCFundingInput]): Unit =
    fundingInputsOpt = Some(fundingInputs)

  override def setChangeAddress(changeAddress: BitcoinAddress): Unit =
    changeAddressOpt = Some(changeAddress)

  override def setPayoutSerialId(payoutSerialId: UInt64): Unit =
    payoutSerialIdOpt = Some(payoutSerialId)

  override def setChangeSerialId(changeSerialId: UInt64): Unit =
    changeSerialIdOpt = Some(changeSerialId)

  override def setNegotiationFields(
      negotiationFields: DLCAccept.NegotiationFields): Unit =
    negotiationFieldsOpt = Some(negotiationFields)
}

case class InMemoryDLCLocalDataStore(
    var fundingSigsOpt: Option[FundingSignatures] = None,
    var cetAdaptorSigsOpt: Option[Vector[(ECPublicKey, ECAdaptorSignature)]] =
      None,
    var refundSigOpt: Option[PartialSignature] = None,
    var isInitiatorOpt: Option[Boolean] = None,
    var fundingPrivKeyOpt: Option[AdaptorSign] = None,
    var fundingUtxosOpt: Option[Vector[ScriptSignatureParams[InputInfo]]] =
      None)
    extends DLCLocalDataStore {

  override def setFundingSigs(fundingSigs: FundingSignatures): Unit =
    fundingSigsOpt = Some(fundingSigs)

  override def setCetAdaptorSigs(
      cetAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)]): Unit =
    cetAdaptorSigsOpt = Some(cetAdaptorSigs)

  override def setRefundSig(refundSig: PartialSignature): Unit =
    refundSigOpt = Some(refundSig)

  override def setIsInitiator(isInitiator: Boolean): Unit =
    isInitiatorOpt = Some(isInitiator)

  override def setFundingPrivKey(fundingPrivKey: AdaptorSign): Unit =
    fundingPrivKeyOpt = Some(fundingPrivKey)

  override def setFundingUtxos(
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]]): Unit =
    fundingUtxosOpt = Some(fundingUtxos)
}

case class InMemoryDLCRemoteDataStore(
    var fundingSigsOpt: Option[FundingSignatures] = None,
    var cetAdaptorSigsOpt: Option[Vector[(ECPublicKey, ECAdaptorSignature)]] =
      None,
    var refundSigOpt: Option[PartialSignature] = None)
    extends DLCRemoteDataStore {

  override def setFundingSigs(fundingSigs: FundingSignatures): Unit =
    fundingSigsOpt = Some(fundingSigs)

  override def setCetAdaptorSigs(
      cetAdaptorSigs: Vector[(ECPublicKey, ECAdaptorSignature)]): Unit =
    cetAdaptorSigsOpt = Some(cetAdaptorSigs)

  override def setRefundSig(refundSig: PartialSignature): Unit =
    refundSigOpt = Some(refundSig)
}
