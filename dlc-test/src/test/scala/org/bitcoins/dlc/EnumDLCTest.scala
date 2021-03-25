package org.bitcoins.dlc

import org.bitcoins.core.protocol.tlv.OracleParamsV0TLV
import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkitcore.util.BitcoinSJvmTest
import org.scalatest.Assertion

import scala.concurrent.Future

class EnumDLCTest extends BitcoinSJvmTest with DLCTest {
  behavior of "Enum DLC"

  val enumOracleSchemesToTest: Vector[(Int, Int)] =
    Vector((1, 1), (1, 2), (2, 2), (2, 3), (3, 5), (5, 8))

  val numEnumOutcomesToTest: Vector[Int] = Vector(2, 3, 5, 8)

  def runSingleNonceTests(
      exec: (Long, Int, Boolean, Int, Int, Option[OracleParamsV0TLV]) => Future[
        Assertion]): Future[Assertion] = {
    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(0.until(numOutcomes).toVector) { outcomeIndex =>
        runTestsForParam(enumOracleSchemesToTest) {
          case (threshold, numOracles) =>
            exec(outcomeIndex, numOutcomes, false, threshold, numOracles, None)
        }
      }
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal enum case" in {
    runSingleNonceTests(executeForCase)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund enum case" in {
    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(enumOracleSchemesToTest) {
        case (threshold, numOracles) =>
          executeRefundCase(numOutcomes = numOutcomes,
                            isMultiNonce = false,
                            oracleThreshold = threshold,
                            numOracles = numOracles)
      }
    }
  }

  it should "all work for a 100 outcome DLC" in {
    val numOutcomes = 100
    val testFs = (0 until 10).map(_ * 10).map { outcomeIndex =>
      for {
        _ <- executeForCase(outcomeIndex,
                            numOutcomes,
                            isMultiDigit = false,
                            oracleThreshold = 1,
                            numOracles = 1)
      } yield succeed
    }

    Future
      .sequence(testFs)
      .flatMap(_ =>
        executeRefundCase(numOutcomes,
                          isMultiNonce = false,
                          oracleThreshold = 1,
                          numOracles = 1))
  }

  it should "be able to derive oracle signature from remote CET signature" in {
    val outcomeIndex = 1

    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(enumOracleSchemesToTest) {
        case (threshold, numOracles) =>
          constructAndSetupDLC(numOutcomes,
                               isMultiDigit = false,
                               oracleThreshold = threshold,
                               numOracles = numOracles).flatMap {
            case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
              val (oracleOutcome, sigs) =
                genOracleOutcomeAndSignatures(numOutcomes,
                                              isNumeric = false,
                                              dlcOffer,
                                              outcomes,
                                              outcomeIndex,
                                              paramsOpt = None)

              assertCorrectSigDerivation(offerSetup = offerSetup,
                                         dlcOffer = dlcOffer,
                                         acceptSetup = acceptSetup,
                                         dlcAccept = dlcAccept,
                                         oracleSigs = sigs,
                                         outcome = oracleOutcome)
          }
      }
    }
  }
}
