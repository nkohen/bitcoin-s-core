package org.bitcoins.dlc

import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.tlv.EnumOutcome
import org.bitcoins.dlc.verify.DLCSignatureVerifier
import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkitcore.util.{BitcoinSJvmTest, BytesUtil}

class DLCValidationTest extends BitcoinSJvmTest with DLCTest {
  behavior of "DLC Validation"

  it should "fail on invalid funding signatures" in {
    val contractParms =
      EnumContractParams(numOutcomes = 3, oracleThreshold = 1, numOracles = 1)
    val (offerClient, acceptClient, _) = constructDLCClients(contractParms)
    val offerVerifier = DLCSignatureVerifier(offerClient.dlcTxBuilder)
    val acceptVerifier = DLCSignatureVerifier(acceptClient.dlcTxBuilder)

    val offerFundingSigs = offerClient.dlcTxSigner.signFundingTx().get
    val acceptFundingSigs = acceptClient.dlcTxSigner.signFundingTx().get

    val badOfferFundingSigs = BytesUtil.flipBit(offerFundingSigs)
    val badAcceptFundingSigs = BytesUtil.flipBit(acceptFundingSigs)

    assert(
      offerClient.dlcTxSigner
        .completeFundingTx(badAcceptFundingSigs)
        .isFailure)
    assert(
      acceptClient.dlcTxSigner
        .completeFundingTx(badOfferFundingSigs)
        .isFailure)

    assert(offerVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
    assert(acceptVerifier.verifyRemoteFundingSigs(offerFundingSigs))

    assert(!offerVerifier.verifyRemoteFundingSigs(badAcceptFundingSigs))
    assert(!acceptVerifier.verifyRemoteFundingSigs(badOfferFundingSigs))
    assert(!offerVerifier.verifyRemoteFundingSigs(offerFundingSigs))
    assert(!acceptVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
  }

  it should "fail on invalid CET signatures" in {
    val contractParms =
      EnumContractParams(numOutcomes = 3, oracleThreshold = 1, numOracles = 1)
    val (offerClient, acceptClient, outcomes) =
      constructDLCClients(contractParms)
    val offerVerifier = DLCSignatureVerifier(offerClient.dlcTxBuilder)
    val acceptVerifier = DLCSignatureVerifier(acceptClient.dlcTxBuilder)

    val contractInfo = offerClient.contractInfo

    val offerCETSigs = offerClient.dlcTxSigner.createCETSigs()
    val acceptCETSigs = acceptClient.dlcTxSigner.createCETSigs()

    val badOfferCETSigs = BytesUtil.flipBit(offerCETSigs)
    val badAcceptCETSigs = BytesUtil.flipBit(acceptCETSigs)

    outcomes.foreach { outcomeUncast =>
      val outcome = outcomeUncast.asInstanceOf[EnumOutcome]
      val oracleInfo =
        contractInfo.oracleInfos.head.asInstanceOf[EnumSingleOracleInfo]
      val oracleOutcome = EnumOracleOutcome(Vector(oracleInfo), outcome)

      val oracleSig = genEnumOracleSignature(oracleInfo, outcome.outcome)

      assertThrows[RuntimeException] {
        offerClient.dlcTxSigner.completeCET(oracleOutcome,
                                            badAcceptCETSigs(oracleOutcome),
                                            Vector(oracleSig))
      }

      assertThrows[RuntimeException] {
        acceptClient.dlcTxSigner
          .completeCET(oracleOutcome,
                       badOfferCETSigs(oracleOutcome),
                       Vector(oracleSig))
      }
    }

    assertThrows[RuntimeException] {
      offerClient.dlcTxSigner.completeRefundTx(badAcceptCETSigs.refundSig)
    }

    assertThrows[RuntimeException] {
      acceptClient.dlcTxSigner.completeRefundTx(badOfferCETSigs.refundSig)
    }

    outcomes.foreach { outcomeUncast =>
      val outcome = EnumOracleOutcome(Vector(
                                        contractInfo.oracleInfos.head
                                          .asInstanceOf[EnumSingleOracleInfo]),
                                      outcomeUncast.asInstanceOf[EnumOutcome])

      assert(offerVerifier.verifyCETSig(outcome, acceptCETSigs(outcome)))
      assert(acceptVerifier.verifyCETSig(outcome, offerCETSigs(outcome)))
    }
    assert(offerVerifier.verifyRefundSig(acceptCETSigs.refundSig))
    assert(offerVerifier.verifyRefundSig(offerCETSigs.refundSig))
    assert(acceptVerifier.verifyRefundSig(offerCETSigs.refundSig))
    assert(acceptVerifier.verifyRefundSig(acceptCETSigs.refundSig))

    outcomes.foreach { outcomeUncast =>
      val outcome = EnumOracleOutcome(Vector(
                                        contractInfo.oracleInfos.head
                                          .asInstanceOf[EnumSingleOracleInfo]),
                                      outcomeUncast.asInstanceOf[EnumOutcome])

      assert(!offerVerifier.verifyCETSig(outcome, badAcceptCETSigs(outcome)))
      assert(!acceptVerifier.verifyCETSig(outcome, badOfferCETSigs(outcome)))

      assert(!offerVerifier.verifyCETSig(outcome, offerCETSigs(outcome)))
      assert(!acceptVerifier.verifyCETSig(outcome, acceptCETSigs(outcome)))
    }
    assert(!offerVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
    assert(!offerVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
    assert(!acceptVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
    assert(!acceptVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
  }
}
