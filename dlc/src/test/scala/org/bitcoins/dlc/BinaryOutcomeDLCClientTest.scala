package org.bitcoins.dlc

import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionConstants,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0
}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, CryptoUtil, FutureUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfoFull,
  P2WPKHV0SpendingInfo
}
import org.bitcoins.testkit.core.gen.{ScriptGenerators, TransactionGenerators}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.{Future, Promise}

class BinaryOutcomeDLCClientTest extends BitcoinSAsyncTest {
  behavior of "BinaryOutcomeDLCClient"

  it should "correctly subtract fees evenly amongst outputs" in {
    // subtractFeeAndSign has an invariant that no EmptyScriptPubKeys are allowed
    val realisticNonEmptyGen = TransactionGenerators.realisticOutput.suchThat(
      _.scriptPubKey != EmptyScriptPubKey)

    // Can't use TransactionGenerators.realisiticOutputs as that can return List.empty
    val nonEmptyRealisticOutputsGen = Gen
      .choose(1, 5)
      .flatMap(n => Gen.listOfN(n, realisticNonEmptyGen))
      .suchThat(_.nonEmpty)

    // CurrencyUnitGenerator.feeRate gives too high of fees
    val feeRateGen = Gen.choose(0, CurrencyUnits.oneBTC.satoshis.toLong).map {
      n =>
        SatoshisPerByte(Satoshis(Int64(n)))
    }

    forAllAsync(nonEmptyRealisticOutputsGen,
                feeRateGen,
                ScriptGenerators.p2pkhScriptPubKey) {
      case (outputs, feeRate, (changeSPK, _)) =>
        val totalInput = outputs.foldLeft(CurrencyUnits.zero) {
          case (accum, output) =>
            accum + output.value
        }

        val inputKey = ECPrivateKey.freshPrivateKey
        val utxos: Vector[BitcoinUTXOSpendingInfoFull] = Vector(
          P2WPKHV0SpendingInfo(
            outPoint =
              TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
            amount = totalInput,
            scriptPubKey = P2WPKHWitnessSPKV0(inputKey.publicKey),
            signer = inputKey,
            hashType = HashType.sigHashAll,
            scriptWitness = P2WPKHWitnessV0(inputKey.publicKey)
          ))
        val network: BitcoinNetwork = RegTest

        val txBuilderF =
          BitcoinTxBuilder(outputs, utxos, feeRate, changeSPK, network)

        val badFeeF = txBuilderF.flatMap { txBuilder =>
          recoverToSucceededIf[IllegalArgumentException](txBuilder.sign)
        }

        for {
          txBuilder <- txBuilderF
          _ <- badFeeF
          tx <- BinaryOutcomeDLCClient.subtractFeeAndSign(txBuilder)
        } yield {
          val diffs = outputs.zip(tx.outputs).map {
            case (before, after) =>
              before.value - after.value
          }

          val firstDiff = diffs.head
          // Fee has been evenly distributed (up to some remainder)
          assert(diffs.forall(diff =>
            diff - firstDiff < Satoshis(Int64(diffs.length))))
        }
    }
  }

  val outcomeWin = "WIN"

  val outcomeWinHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
  val outcomeLose = "LOSE"

  val outcomeLoseHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip
  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: ECPublicKey = oraclePrivKey.publicKey
  val preCommittedK: SchnorrNonce = SchnorrNonce.freshNonce
  val preCommittedR: ECPublicKey = preCommittedK.publicKey
  val localInput: CurrencyUnit = CurrencyUnits.oneBTC
  val remoteInput: CurrencyUnit = CurrencyUnits.oneBTC

  val inputPrivKeyLocal: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal: ECPublicKey = inputPrivKeyLocal.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey

  val blockTimeToday: BlockTime = BlockTime(
    UInt32(System.currentTimeMillis() / 1000))

  val localFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(localInput * 2,
                        P2WPKHWitnessSPKV0(inputPrivKeyLocal.publicKey))),
    UInt32.zero
  )

  val localFundingUtxos = Vector(
    P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(localFundingTx.txId, UInt32.zero),
      amount = localInput * 2,
      scriptPubKey = P2WPKHWitnessSPKV0(inputPubKeyLocal),
      signer = inputPrivKeyLocal,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(inputPrivKeyLocal.publicKey)
    )
  )

  val remoteFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(remoteInput * 2,
                        P2WPKHWitnessSPKV0(inputPrivKeyRemote.publicKey))),
    UInt32.zero
  )

  val remoteFundingUtxos = Vector(
    P2WPKHV0SpendingInfo(
      outPoint = TransactionOutPoint(remoteFundingTx.txId, UInt32.zero),
      amount = remoteInput * 2,
      scriptPubKey = P2WPKHWitnessSPKV0(inputPubKeyRemote),
      signer = inputPrivKeyRemote,
      hashType = HashType.sigHashAll,
      scriptWitness = P2WPKHWitnessV0(inputPrivKeyRemote.publicKey)
    )
  )

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val offerExtPrivKey: ExtPrivateKey =
    ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)

  val acceptExtPrivKey: ExtPrivateKey =
    ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)

  val localFundingInputs: Vector[(Transaction, UInt32)] = Vector(
    (localFundingTx, UInt32.zero))

  val remoteFundingInputs: Vector[(Transaction, UInt32)] = Vector(
    (remoteFundingTx, UInt32.zero))

  val timeouts: DLCTimeouts = DLCTimeouts(0, blockTimeToday, blockTimeToday)
  val feeRate: SatoshisPerByte = SatoshisPerByte(Satoshis.one)

  // Offer is local
  val dlcOffer: BinaryOutcomeDLCClient = BinaryOutcomeDLCClient(
    outcomeWin = outcomeWin,
    outcomeLose = outcomeLose,
    oraclePubKey = oraclePubKey,
    preCommittedR = preCommittedR,
    isInitiator = true,
    extPrivKey = offerExtPrivKey,
    remoteExtPubKey = acceptExtPrivKey.extPublicKey,
    input = localInput,
    remoteInput = remoteInput,
    fundingUtxos = localFundingUtxos,
    remoteFundingInputs = remoteFundingInputs,
    winPayout = localInput + CurrencyUnits.oneMBTC,
    losePayout = localInput - CurrencyUnits.oneMBTC,
    timeouts = timeouts,
    feeRate = feeRate,
    changeSPK = localChangeSPK,
    remoteChangeSPK = remoteChangeSPK,
    network = RegTest
  )

  // Accept is remote
  val dlcAccept: BinaryOutcomeDLCClient = BinaryOutcomeDLCClient(
    outcomeWin = outcomeWin,
    outcomeLose = outcomeLose,
    oraclePubKey = oraclePubKey,
    preCommittedR = preCommittedR,
    isInitiator = false,
    extPrivKey = acceptExtPrivKey,
    remoteExtPubKey = offerExtPrivKey.extPublicKey,
    input = remoteInput,
    remoteInput = localInput,
    fundingUtxos = remoteFundingUtxos,
    remoteFundingInputs = localFundingInputs,
    winPayout = remoteInput - CurrencyUnits.oneMBTC,
    losePayout = remoteInput + CurrencyUnits.oneMBTC,
    timeouts = timeouts,
    feeRate = feeRate,
    changeSPK = remoteChangeSPK,
    remoteChangeSPK = localChangeSPK,
    network = RegTest
  )

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(outcome: DLCOutcome): Assertion = {
    val DLCOutcome(fundingTx, cet, closingTx, cetSpendingInfo) = outcome

    assert(noEmptySPKOutputs(fundingTx))
    assert(noEmptySPKOutputs(cet))
    assert(noEmptySPKOutputs(closingTx))

    assert(
      BitcoinScriptUtil.verifyScript(closingTx, Vector(cetSpendingInfo))
    )
  }

  def setupDLC(): Future[(SetupDLC, SetupDLC)] = {
    val offerSigReceiveP =
      Promise[(PartialSignature, PartialSignature, PartialSignature)]()
    val sendAcceptSigs = {
      (
          sig1: PartialSignature,
          sig2: PartialSignature,
          sig3: PartialSignature) =>
        val _ = offerSigReceiveP.success(sig1, sig2, sig3)
        FutureUtil.unit
    }

    val acceptSigReceiveP = Promise[(
        PartialSignature,
        PartialSignature,
        PartialSignature,
        Vector[PartialSignature])]()
    val sendOfferSigs = {
      (
          sig1: PartialSignature,
          sig2: PartialSignature,
          sig3: PartialSignature,
          sigs: Vector[PartialSignature]) =>
        val _ = acceptSigReceiveP.success(sig1, sig2, sig3, sigs)
        FutureUtil.unit
    }

    val acceptSetupF = dlcAccept.setupDLCAccept(sendSigs = sendAcceptSigs,
                                                getSigs =
                                                  acceptSigReceiveP.future)
    val offerSetupF = dlcOffer.setupDLCOffer(getSigs = offerSigReceiveP.future,
                                             sendSigs = sendOfferSigs,
                                             getFundingTx =
                                               acceptSetupF.map(_.fundingTx))

    for {
      acceptSetup <- acceptSetupF
      offerSetup <- offerSetupF
    } yield (acceptSetup, offerSetup)
  }

  def executeUnilateralForCase(
      outcomeHash: Sha256DigestBE,
      local: Boolean): Future[Assertion] = {
    val oracleSig =
      Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

    setupDLC().flatMap {
      case (acceptSetup, offerSetup) =>
        val (unilateralSetup, unilateralDLC, otherSetup, otherDLC) =
          if (local) {
            (offerSetup, dlcOffer, acceptSetup, dlcAccept)
          } else {
            (acceptSetup, dlcAccept, offerSetup, dlcOffer)
          }

        for {
          unilateralOutcome <- unilateralDLC.executeUnilateralDLC(
            unilateralSetup,
            Future.successful(oracleSig))
          otherOutcome <- otherDLC.executeRemoteUnilateralDLC(
            otherSetup,
            unilateralOutcome.cet)
        } yield {
          validateOutcome(unilateralOutcome)
          validateOutcome(otherOutcome)
        }
    }
  }

  def executeRefundCase(): Future[Assertion] = {
    setupDLC().flatMap {
      case (acceptSetup, offerSetup) =>
        for {
          acceptOutcome <- dlcAccept.executeRefundDLC(acceptSetup)
          offerOutcome <- dlcOffer.executeRefundDLC(offerSetup)
        } yield {
          validateOutcome(acceptOutcome)
          validateOutcome(offerOutcome)

          assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
          assert(acceptOutcome.cet == offerOutcome.cet)
        }
    }
  }
  /*
  def executeJusticeCase(
      fakeWin: Boolean,
      local: Boolean): Future[Assertion] = {
    dlc.setupDLC().flatMap { setup =>
      val timedOutCET = if (fakeWin) {
        if (local) {
          setup.cetWinRemote
        } else {
          setup.cetWinLocal
        }
      } else {
        if (local) {
          setup.cetLoseRemote
        } else {
          setup.cetLoseLocal
        }
      }
      val outcomeF = dlc.executeJusticeDLC(setup, timedOutCET, local)

      outcomeF.map(validateOutcome)
    }
  }
   */
  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal win case" in {
    for {
      _ <- executeUnilateralForCase(outcomeWinHash, local = true)
      _ <- executeUnilateralForCase(outcomeWinHash, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal lose case" in {
    for {
      _ <- executeUnilateralForCase(outcomeLoseHash, local = true)
      _ <- executeUnilateralForCase(outcomeLoseHash, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    executeRefundCase()
  }
  /*
  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice win case" in {
    for {
      _ <- executeJusticeCase(fakeWin = true, local = true)
      _ <- executeJusticeCase(fakeWin = true, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice lose case" in {
    for {
      _ <- executeJusticeCase(fakeWin = false, local = true)
      _ <- executeJusticeCase(fakeWin = false, local = false)
    } yield succeed
  }*/
}
