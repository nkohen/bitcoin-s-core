package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.DLCStatus.{Claimed, RemoteClaimed}
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto._
import org.bitcoins.testkit.dlc.DLCTest.genNumericOracleOutcome
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

import scala.util.Random

class DLCMultiOracleExactNumericExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  behavior of "DLCWallet"

  val privateKeys: Vector[ECPrivateKey] =
    0.until(5).map(_ => ECPrivateKey.freshPrivateKey).toVector

  val kValues: Vector[Vector[ECPrivateKey]] =
    privateKeys.map(_ =>
      0.until(numDigits).map(_ => ECPrivateKey.freshPrivateKey).toVector)

  val contractDescriptor: NumericContractDescriptor =
    DLCWalletUtil.multiNonceContractDescriptor

  val announcements: Vector[OracleAnnouncementTLV] =
    privateKeys.zip(kValues).map {
      case (priv, ks) =>
        OracleAnnouncementV0TLV.dummyForKeys(priv, ks.map(_.schnorrNonce))
    }

  val threshold = 3

  val oracleInfo: NumericExactMultiOracleInfo =
    NumericExactMultiOracleInfo(threshold, announcements)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, contractDescriptor, oracleInfo)
  }

  def getSigs(contractInfo: ContractInfo): (
      Vector[OracleAttestmentTLV],
      Vector[OracleAttestmentTLV]) = {
    contractInfo.contractDescriptor match {
      case _: NumericContractDescriptor => ()
      case _: EnumContractDescriptor =>
        throw new IllegalArgumentException("Unexpected Contract Info")
    }

    val oracleIndices =
      0.until(oracleInfo.numOracles).toVector
    val initChosenOracles =
      Random.shuffle(oracleIndices).take(oracleInfo.threshold).sorted

    val initiatorWinOutcome =
      contractInfo.allOutcomesAndPayouts
        .maxBy(_._2.toLong)
        ._1
        .outcome
        .asInstanceOf[UnsignedNumericOutcome]

    val initiatorWinVec = initiatorWinOutcome.digits

    val initWinOutcomes: NumericOracleOutcome = genNumericOracleOutcome(
      initChosenOracles,
      contractInfo,
      initiatorWinVec,
      None)

    val initiatorWinSigs = privateKeys.zip(kValues).flatMap {
      case (priv, kValues) =>
        val outcomeOpt = initWinOutcomes.oraclesAndOutcomes.find(
          _._1.publicKey == priv.schnorrPublicKey)

        outcomeOpt.map { outcome =>
          val sigs = outcome._2.digits.zip(kValues).map {
            case (num, kValue) =>
              val hash = CryptoUtil.sha256DLCAttestation(num.toString).bytes
              priv.schnorrSignWithNonce(hash, kValue)
          }

          OracleAttestmentV0TLV(priv.schnorrPublicKey, sigs)
        }
    }

    val recipientChosenOracles =
      Random.shuffle(oracleIndices).take(oracleInfo.threshold).sorted

    val recipientWinOutcome =
      contractInfo.allOutcomesAndPayouts
        .find(_._2 == Satoshis.zero)
        .get
        ._1
        .outcome
        .asInstanceOf[UnsignedNumericOutcome]

    val recipientWinVec = recipientWinOutcome.digits

    val recipientWinOutcomes: NumericOracleOutcome = genNumericOracleOutcome(
      recipientChosenOracles,
      contractInfo,
      recipientWinVec,
      None)

    val recipientWinSigs = privateKeys.zip(kValues).flatMap {
      case (priv, kValues) =>
        val outcomeOpt = recipientWinOutcomes.oraclesAndOutcomes.find(
          _._1.publicKey == priv.schnorrPublicKey)

        outcomeOpt.map { outcome =>
          val sigs = outcome._2.digits.zip(kValues).map {
            case (num, kValue) =>
              val hash = CryptoUtil.sha256DLCAttestation(num.toString).bytes
              priv.schnorrSignWithNonce(hash, kValue)
          }

          OracleAttestmentV0TLV(priv.schnorrPublicKey, sigs)
        }
    }

    // Shuffle to make sure ordering doesn't matter
    (Random.shuffle(initiatorWinSigs), Random.shuffle(recipientWinSigs))
  }

  it must "execute as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      offer <- getInitialOffer(wallets._1.wallet)
      (sigs, _) = getSigs(offer.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sigs)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Claimed), Some(statusB: RemoteClaimed)) =>
            verifyingMatchingOracleSigs(statusA, statusB)
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Claimed)
          assert(dlcB.state == DLCState.RemoteClaimed)
        case (_, _) => fail()
      }
    }
  }

  it must "execute as the recipient" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      offer <- getInitialOffer(wallets._2.wallet)
      (_, sigs) = getSigs(offer.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sigs)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: RemoteClaimed), Some(statusB: Claimed)) =>
            verifyingMatchingOracleSigs(statusB, statusA)
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.RemoteClaimed)
          assert(dlcB.state == DLCState.Claimed)
        case (_, _) => fail()
      }
    }
  }

  private def verifyingMatchingOracleSigs(
      statusA: Claimed,
      statusB: RemoteClaimed): Boolean = {
    val outcome = statusB.outcome
    val numSigs = outcome match {
      case EnumOutcome(outcome) =>
        throw new RuntimeException(s"Unexpected outcome type, got $outcome")
      case UnsignedNumericOutcome(digits) => digits.size
    }

    val aggR = statusA.oracleSigs
      .take(numSigs)
      .map(_.rx.publicKey)
      .reduce(_.add(_))
      .schnorrNonce

    val aggS = statusA.oracleSigs
      .take(numSigs)
      .map(_.sig)
      .reduce(_.add(_))

    val aggregateSignature =
      SchnorrDigitalSignature(aggR, aggS)
    aggregateSignature == statusB.oracleSig
  }
}
