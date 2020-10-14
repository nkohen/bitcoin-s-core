package org.bitcoins.dlc.builder

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.{
  DLCFundingInput,
  DLCPublicKeys,
  DLCTimeouts
}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionOutPoint,
  WitnessTransaction
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._

import scala.concurrent.{ExecutionContext, Future}

case class DLCTxBuilder[Outcome](
    offer: DLCOffer[Outcome],
    accept: DLCAcceptWithoutSigs)(implicit ec: ExecutionContext) {

  val DLCOffer(offerOutcomes: ContractInfo[Outcome],
               oracleInfo: OracleInfo[Outcome],
               DLCPublicKeys(offerFundingKey: ECPublicKey,
                             offerFinalAddress: BitcoinAddress),
               offerTotalCollateral: Satoshis,
               offerFundingInputs: Vector[DLCFundingInput],
               offerChangeAddress: BitcoinAddress,
               feeRate: SatoshisPerVirtualByte,
               DLCTimeouts(contractMaturity: BlockTimeStamp,
                           contractTimeout: BlockTimeStamp)) = offer

  val network: BitcoinNetwork = offerFinalAddress.networkParameters match {
    case network: BitcoinNetwork => network
  }

  val DLCAcceptWithoutSigs(acceptTotalCollateral: Satoshis,
                           DLCPublicKeys(acceptFundingKey: ECPublicKey,
                                         acceptFinalAddress: BitcoinAddress),
                           acceptFundingInputs: Vector[DLCFundingInput],
                           acceptChangeAddress: BitcoinAddress,
                           tempContractId: Sha256Digest) = accept

  val totalInput: CurrencyUnit = offerTotalCollateral + acceptTotalCollateral

  val offerTotalFunding: CurrencyUnit =
    offerFundingInputs.map(_.output.value).sum

  val acceptTotalFunding: CurrencyUnit =
    acceptFundingInputs.map(_.output.value).sum

  require(offer.tempContractId == tempContractId,
          "Offer and accept (without sigs) must refer to same event")
  require(acceptFinalAddress.networkParameters == network,
          "Offer and accept (without sigs) must be on the same network")
  require(offerChangeAddress.networkParameters == network,
          "Offer change address must have same network as final address")
  require(acceptChangeAddress.networkParameters == network,
          "Accept change address must have same network as final address")
  require(
    !offerOutcomes
      .isInstanceOf[SingleNonceContractInfo] || totalInput >= offerOutcomes
      .asInstanceOf[SingleNonceContractInfo]
      .values
      .max,
    "Total collateral must add up to max winnings"
  )
  require(
    offerTotalFunding >= offerTotalCollateral,
    "Offer funding inputs must add up to at least offer's total collateral")
  require(
    acceptTotalFunding >= acceptTotalCollateral,
    "Accept funding inputs must add up to at least accept's total collateral")

  val acceptOutcomes: ContractInfo[Outcome] =
    offerOutcomes.flip(totalInput.satoshis)

  val oracleAndContractInfo: OracleAndContractInfo[Outcome] =
    OracleAndContractInfo(oracleInfo, offerOutcomes)

  /** Returns the payouts for the signature as (toOffer, toAccept) */
  def getPayouts(oracleSigs: Vector[SchnorrDigitalSignature]): (
      CurrencyUnit,
      CurrencyUnit) = {
    DLCTxBuilder.getPayouts(oracleSigs,
                            oracleInfo,
                            offerOutcomes,
                            acceptOutcomes)
  }

  lazy val fundingTxBuilder: DLCFundingTxBuilder = {
    DLCFundingTxBuilder(
      offerFundingKey = offerFundingKey,
      acceptFundingKey = acceptFundingKey,
      feeRate = feeRate,
      offerInput = offerTotalCollateral,
      acceptInput = acceptTotalCollateral,
      offerFundingInputs = offerFundingInputs,
      acceptFundingInputs = acceptFundingInputs,
      offerChangeSPK = offerChangeAddress.scriptPubKey,
      acceptChangeSPK = acceptChangeAddress.scriptPubKey,
      offerPayoutSPK = offerFinalAddress.scriptPubKey,
      acceptPayoutSPK = acceptFinalAddress.scriptPubKey
    )
  }

  /** Constructs the unsigned funding transaction */
  lazy val buildFundingTx: Future[Transaction] = {
    fundingTxBuilder.buildFundingTx()
  }

  private lazy val cetBuilderF = {
    for {
      fundingTx <- buildFundingTx
    } yield {
      val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
      val fundingOutputRef =
        OutputReference(fundingOutPoint, fundingTx.outputs.head)

      DLCCETBuilder(
        offerOutcomes = offerOutcomes,
        acceptOutcomes = acceptOutcomes,
        offerFundingKey = offerFundingKey,
        offerFinalSPK = offerFinalAddress.scriptPubKey,
        acceptFundingKey = acceptFundingKey,
        acceptFinalSPK = acceptFinalAddress.scriptPubKey,
        timeouts = offer.timeouts,
        feeRate = feeRate,
        oracleInfo = offer.oracleInfo,
        fundingOutputRef = fundingOutputRef
      )
    }
  }

  /** Constructs the unsigned Contract Execution Transaction (CET)
    * for a given outcome hash
    */
  def buildCET(outcome: Outcome): Future[WitnessTransaction] = {
    for {
      cetBuilder <- cetBuilderF
      cet <- cetBuilder.buildCET(outcome)
    } yield cet
  }

  /** Constructs the unsigned refund transaction */
  lazy val buildRefundTx: Future[WitnessTransaction] = {
    val builderF = for {
      fundingTx <- buildFundingTx
    } yield {
      val fundingOutPoint = TransactionOutPoint(fundingTx.txId, UInt32.zero)
      val fundingOutputRef =
        OutputReference(fundingOutPoint, fundingTx.outputs.head)

      DLCRefundTxBuilder(
        offerTotalCollateral,
        offerFundingKey,
        offerFinalAddress.scriptPubKey,
        acceptTotalCollateral,
        acceptFundingKey,
        acceptFinalAddress.scriptPubKey,
        fundingOutputRef,
        offer.timeouts,
        feeRate
      )
    }

    builderF.flatMap(_.buildRefundTx())
  }
}

object DLCTxBuilder {

  /** Returns the payouts for the signature as (toOffer, toAccept) */
  def getPayouts[Outcome](
      oracleSigs: Vector[SchnorrDigitalSignature],
      oracleInfo: OracleInfo,
      offerOutcomes: ContractInfo[Outcome],
      acceptOutcomes: ContractInfo[Outcome]): (CurrencyUnit, CurrencyUnit) = {
    val outcome =
      OracleAndContractInfo(oracleInfo, offerOutcomes).outcomeFromSignatures(
        oracleSigs)
    (offerOutcomes(outcome), acceptOutcomes(outcome))
  }
}
