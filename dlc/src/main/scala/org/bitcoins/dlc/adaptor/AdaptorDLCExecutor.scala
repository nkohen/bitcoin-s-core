package org.bitcoins.dlc.adaptor

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.crypto.{SchnorrDigitalSignature, Sha256DigestBE}

import scala.concurrent.{ExecutionContext, Future}

/** Responsible for constructing SetupDLCs and DLCOutcomes */
case class AdaptorDLCExecutor(signer: AdaptorDLCTxSigner)(implicit
    ec: ExecutionContext) {
  val builder: AdaptorDLCTxBuilder = signer.builder
  val isInitiator: Boolean = signer.isInitiator
  val messages: Vector[Sha256DigestBE] = builder.offerOutcomes.keys.toVector

  /** Constructs the initiator's SetupDLC given the non-initiator's
    * CETSignatures which should arrive in a DLC accept message
    */
  def setupDLCOffer(cetSigs: AdaptorCETSignatures): Future[SetupAdaptorDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    setupDLC(cetSigs, None)
  }

  /** Constructs the non-initiator's SetupDLC given the initiator's
    * CETSignatures and FundingSignatures which should arrive in
    * a DLC sign message
    */
  def setupDLCAccept(
      cetSigs: AdaptorCETSignatures,
      fundingSigs: AdaptorFundingSignatures): Future[SetupAdaptorDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    setupDLC(cetSigs, Some(fundingSigs))
  }

  /** Constructs a SetupDLC given the necessary signature information
    * from the counter-party.
    */
  def setupDLC(
      cetSigs: AdaptorCETSignatures,
      fundingSigsOpt: Option[AdaptorFundingSignatures]): Future[
    SetupAdaptorDLC] = {
    if (!isInitiator) {
      require(fundingSigsOpt.isDefined,
              "Accepting party must provide remote funding signatures")
    }

    val AdaptorCETSignatures(outcomeSigs, refundSig) = cetSigs
    val cetInfoFs = outcomeSigs.map {
      case (msg, remoteAdaptorSig) =>
        builder.buildCET(msg).map { cet =>
          msg -> AdaptorCETInfo(cet, remoteAdaptorSig)
        }
    }

    for {
      fundingTx <- {
        fundingSigsOpt match {
          case Some(fundingSigs) => signer.signFundingTx(fundingSigs)
          case None              => builder.buildFundingTx
        }
      }
      cetInfos <- Future.sequence(cetInfoFs)
      refundTx <- signer.signRefundTx(refundSig)
    } yield {
      SetupAdaptorDLC(fundingTx, cetInfos.toMap, refundTx)
    }
  }

  /** Return's this party's payout for a given oracle signature */
  def getPayout(sig: SchnorrDigitalSignature): CurrencyUnit = {
    signer.getPayout(sig)
  }

  def executeDLC(
      dlcSetup: SetupAdaptorDLC,
      oracleSig: SchnorrDigitalSignature): Future[ExecutedDLCOutcome] = {
    val SetupAdaptorDLC(fundingTx, cetInfos, _) = dlcSetup

    val msgOpt =
      messages.find(msg => builder.oraclePubKey.verify(msg.bytes, oracleSig))
    val (msg, remoteAdaptorSig) = msgOpt match {
      case Some(msg) =>
        val cetInfo = cetInfos(msg)
        (msg, cetInfo.remoteSignature)
      case None =>
        throw new IllegalArgumentException(
          s"Signature does not correspond to any possible outcome! $oracleSig")
    }

    signer.signCET(msg, remoteAdaptorSig, oracleSig).map { cet =>
      ExecutedDLCOutcome(fundingTx, cet)
    }
  }

  def executeRefundDLC(dlcSetup: SetupAdaptorDLC): RefundDLCOutcome = {
    val SetupAdaptorDLC(fundingTx, _, refundTx) = dlcSetup
    RefundDLCOutcome(fundingTx, refundTx)
  }
}
