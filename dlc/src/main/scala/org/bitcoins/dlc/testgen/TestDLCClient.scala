package org.bitcoins.dlc.testgen

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt64
import org.bitcoins.core.protocol.dlc.DLCMessage.DLCAccept
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.dlc.data.InMemoryDLCDataStore
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, ScriptPubKey}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution.{
  DLCExecutor,
  ExecutedDLCOutcome,
  RefundDLCOutcome,
  SetupDLC
}
import org.bitcoins.dlc.sign.DLCTxSigner

import scala.concurrent.{ExecutionContext, Future}

/** This case class allows for the construction and execution of
  * Discreet Log Contracts between two parties running on this machine (for tests).
  */
case class TestDLCClient(dataStore: InMemoryDLCDataStore)(implicit
    ec: ExecutionContext) {

  val dlcTxBuilder: DLCTxBuilder = DLCTxBuilder(dataStore)

  val dlcTxSigner: DLCTxSigner = DLCTxSigner(dlcTxBuilder)

  val isInitiator: Boolean = dlcTxSigner.isInitiator

  lazy val fundingPrivKey: AdaptorSign = dataStore.getter.local.fundingPrivKey

  private val dlcExecutor = DLCExecutor(dlcTxSigner)

  lazy val contractInfo: ContractInfo = dataStore.getter.global.contractInfo

  val messages: Vector[OracleOutcome] =
    dataStore.getter.global.contractInfo.allOutcomes

  val timeouts: DLCTimeouts = dataStore.getter.global.timeouts

  def fundingTx: Transaction = dlcTxBuilder.buildFundingTx

  lazy val fundingTxIdBE: DoubleSha256DigestBE = fundingTx.txIdBE

  /** Sets up the non-initiator's DLC given functions for sending
    * CETSignatures to the initiator as well as receiving CETSignatures
    * and FundingSignatures from them
    */
  def setupDLCAccept(
      sendSigs: CETSignatures => Future[Unit],
      getSigs: Future[(CETSignatures, FundingSignatures)]): Future[SetupDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    val (remoteCetSigs, cets) = dlcTxSigner.createCETsAndCETSigs()

    for {
      _ <- sendSigs(remoteCetSigs)
      (cetSigs, fundingSigs) <- getSigs
      setupDLC <- Future.fromTry {
        dlcExecutor.setupDLCAccept(cetSigs, fundingSigs, Some(cets))
      }
    } yield {
      setupDLC
    }
  }

  /** Sets up the initiator's DLC given functions for getting CETSignatures
    * from the non-initiator as well as sending signatures to them, and lastly
    * a Future which will be populated with the broadcasted (or relayed) fully
    * signed funding transaction
    */
  def setupDLCOffer(
      getSigs: Future[CETSignatures],
      sendSigs: (CETSignatures, FundingSignatures) => Future[Unit],
      getFundingTx: Future[Transaction]): Future[SetupDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    for {
      cetSigs <- getSigs
      setupDLCWithoutFundingTxSigs <- Future.fromTry {
        dlcExecutor.setupDLCOffer(cetSigs)
      }
      cetSigs =
        dlcTxSigner.createCETSigs(setupDLCWithoutFundingTxSigs.cets.map {
          case (msg, info) => OutcomeCETPair(msg, info.tx)
        })
      localFundingSigs <- Future.fromTry {
        dlcTxSigner.signFundingTx()
      }
      _ <- sendSigs(cetSigs, localFundingSigs)
      fundingTx <- getFundingTx
    } yield {
      setupDLCWithoutFundingTxSigs.copy(fundingTx = fundingTx)
    }
  }

  def executeDLC(
      dlcSetup: SetupDLC,
      oracleSigsF: Future[Vector[OracleSignatures]]): Future[
    ExecutedDLCOutcome] = {
    oracleSigsF.map { oracleSigs =>
      dlcExecutor.executeDLC(dlcSetup, oracleSigs)
    }
  }

  def executeRefundDLC(dlcSetup: SetupDLC): RefundDLCOutcome = {
    dlcExecutor.executeRefundDLC(dlcSetup)
  }
}

object TestDLCClient {

  def apply(
      offer: DLCMessage.DLCOffer,
      accept: DLCMessage.DLCAcceptWithoutSigs,
      isInitiator: Boolean,
      fundingPrivKey: ECPrivateKey,
      payoutPrivKey: ECPrivateKey,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
      ec: ExecutionContext): TestDLCClient = {
    val dataStore = InMemoryDLCDataStore()
    dataStore.writeOffer(offer)
    dataStore.writeAcceptWithoutSigs(accept)
    dataStore.local.setIsInitiator(isInitiator)
    dataStore.local.setFundingPrivKey(fundingPrivKey)
    dataStore.local.setFundingUtxos(fundingUtxos)

    val finalAddress =
      Bech32Address(P2WPKHWitnessSPKV0(payoutPrivKey.publicKey), RegTest)
    if (isInitiator) {
      dataStore.offer.setFinalAddress(finalAddress)
    } else {
      dataStore.accept.setFinalAddress(finalAddress)
    }

    TestDLCClient(dataStore)
  }

  // TODO: just directly write to datastore
  def apply(
      outcomes: ContractInfo,
      isInitiator: Boolean,
      fundingPrivKey: ECPrivateKey,
      payoutPrivKey: ECPrivateKey,
      payoutSerialId: UInt64,
      remotePubKeys: DLCPublicKeys,
      remotePayoutSerialId: UInt64,
      input: CurrencyUnit,
      remoteInput: CurrencyUnit,
      fundingUtxos: Vector[SpendingInfoWithSerialId],
      remoteFundingInputs: Vector[DLCFundingInput],
      timeouts: DLCTimeouts,
      feeRate: SatoshisPerVirtualByte,
      changeSPK: ScriptPubKey,
      changeSerialId: UInt64,
      remoteChangeSPK: ScriptPubKey,
      remoteChangeSerialId: UInt64,
      fundOutputSerialId: UInt64)(implicit
      ec: ExecutionContext): TestDLCClient = {
    val network = remotePubKeys.payoutAddress.networkParameters

    val remoteOutcomes: ContractInfo = {
      val descriptors =
        outcomes.contractDescriptors.map(_.flip((input + remoteInput).satoshis))

      val contracts = descriptors.zip(outcomes.oracleInfos).map {
        case (descriptor, oracleInfo) =>
          val pair =
            ContractOraclePair.fromDescriptorOracle(descriptor, oracleInfo)
          SingleContractInfo(outcomes.totalCollateral, pair)
      }

      outcomes match {
        case _: SingleContractInfo => contracts.head
        case _: DisjointUnionContractInfo =>
          DisjointUnionContractInfo(contracts)
      }
    }

    val offerOutcomes = if (isInitiator) outcomes else remoteOutcomes

    val finalAddress =
      Bech32Address(P2WPKHWitnessSPKV0(payoutPrivKey.publicKey), network)
    val changeAddress = BitcoinAddress.fromScriptPubKey(changeSPK, network)
    val remoteChangeAddress =
      BitcoinAddress.fromScriptPubKey(remoteChangeSPK, network)

    val negotiationFields = offerOutcomes match {
      case _: SingleContractInfo => DLCAccept.NoNegotiationFields
      case DisjointUnionContractInfo(contracts) =>
        DLCAccept.NegotiationFieldsV2(
          contracts.map(_ => DLCAccept.NoNegotiationFields))
    }

    val dataStore = InMemoryDLCDataStore()

    val (localParty, remoteParty) = if (isInitiator) {
      (dataStore.offer, dataStore.accept)
    } else {
      (dataStore.accept, dataStore.offer)
    }

    dataStore.global.setContractInfo(offerOutcomes)
    dataStore.local.setIsInitiator(isInitiator)
    dataStore.local.setFundingPrivKey(fundingPrivKey)
    localParty.setFundingKey(fundingPrivKey.publicKey)
    localParty.setFinalAddress(finalAddress)
    localParty.setPayoutSerialId(payoutSerialId)
    remoteParty.setFundingKey(remotePubKeys.fundingKey)
    remoteParty.setFinalAddress(remotePubKeys.payoutAddress)
    remoteParty.setPayoutSerialId(remotePayoutSerialId)
    localParty.setCollateral(input.satoshis)
    remoteParty.setCollateral(remoteInput.satoshis)
    dataStore.local.setFundingUtxos(fundingUtxos.map(_.spendingInfo))
    localParty.setFundingInputs(fundingUtxos.map(_.toDLCFundingInput))
    remoteParty.setFundingInputs(remoteFundingInputs)
    dataStore.global.setTimeouts(timeouts)
    dataStore.global.setFeeRate(feeRate)
    localParty.setChangeAddress(changeAddress)
    localParty.setChangeSerialId(changeSerialId)
    remoteParty.setChangeAddress(remoteChangeAddress)
    remoteParty.setChangeSerialId(remoteChangeSerialId)
    dataStore.global.setFundOutputSerialId(fundOutputSerialId)
    dataStore.accept.setNegotiationFields(negotiationFields)

    val offer = dataStore.getter.getOffer

    dataStore.global.setTempContractId(offer.tempContractId)

    TestDLCClient(dataStore)
  }
}
