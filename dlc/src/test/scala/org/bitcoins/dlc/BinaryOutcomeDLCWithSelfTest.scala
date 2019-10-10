package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{DoubleSha256DigestBE, ECPrivateKey, Schnorr}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class BinaryOutcomeDLCWithSelfTest extends BitcoinSAsyncTest {
  behavior of "BinaryOutcomeDLCWithSelf"

  it should "work" in {
    val outcomeWin = "WIN"
    val outcomeWinHash = CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
    val outcomeLose = "LOSE"
    val outcomeLoseHash =
      CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip
    val oraclePrivKey = ECPrivateKey.freshPrivateKey
    val oraclePubKey = oraclePrivKey.publicKey
    val preCommittedK = ECPrivateKey.freshPrivateKey
    val preCommittedR = preCommittedK.publicKey
    val fundingLocalPrivKey = ECPrivateKey.freshPrivateKey
    val fundingRemotePrivKey = ECPrivateKey.freshPrivateKey
    val cetLocalPrivKey = ECPrivateKey.freshPrivateKey
    val cetRemotePrivKey = ECPrivateKey.freshPrivateKey
    val finalLocalPrivKey = ECPrivateKey.freshPrivateKey
    val finalRemotePrivKey = ECPrivateKey.freshPrivateKey
    val localInput = CurrencyUnits.oneBTC
    val remoteInput = CurrencyUnits.oneBTC

    val inputPrivKeyLocal = ECPrivateKey.freshPrivateKey
    val inputPubKeyLocal = inputPrivKeyLocal.publicKey
    val inputPrivKeyRemote = ECPrivateKey.freshPrivateKey
    val inputPubKeyRemote = inputPrivKeyRemote.publicKey

    val localFundingUtxos = Vector(
      BitcoinUTXOSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
        output = TransactionOutput(localInput + (Satoshis.one * 375),
                                   P2PKHScriptPubKey(inputPubKeyLocal)),
        signers = Vector(inputPrivKeyLocal),
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        hashType = HashType.sigHashAll
      ))

    val remoteFundingUtxos = Vector(
      BitcoinUTXOSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
        output = TransactionOutput(remoteInput + (Satoshis.one * 375),
                                   P2PKHScriptPubKey(inputPubKeyRemote)),
        signers = Vector(inputPrivKeyRemote),
        redeemScriptOpt = None,
        scriptWitnessOpt = None,
        hashType = HashType.sigHashAll
      ))

    val localWinPayout = localInput + CurrencyUnits.oneMBTC
    val remoteWinPayout = remoteInput - CurrencyUnits.oneMBTC
    val localLosePayout = localInput - CurrencyUnits.oneMBTC
    val remoteLosePayout = remoteInput + CurrencyUnits.oneMBTC
    val timeout = 1.day.toMillis.toInt
    val feeRate = SatoshisPerByte(Satoshis.one)
    val changePrivKey = ECPrivateKey.freshPrivateKey
    val changePubKey = changePrivKey.publicKey
    val changeSPK = P2PKHScriptPubKey(changePubKey)
    val network = RegTest

    val oracleSig = Schnorr.sign(outcomeWinHash.bytes, oraclePrivKey)

    val dlc = BinaryOutcomeDLCWithSelf(
      outcomeWin,
      outcomeLose,
      oraclePubKey,
      preCommittedR,
      fundingLocalPrivKey,
      fundingRemotePrivKey,
      cetLocalPrivKey,
      cetRemotePrivKey,
      finalLocalPrivKey,
      finalRemotePrivKey,
      localInput,
      remoteInput,
      localFundingUtxos,
      remoteFundingUtxos,
      localWinPayout,
      remoteWinPayout,
      localLosePayout,
      remoteLosePayout,
      timeout,
      feeRate,
      changeSPK,
      network
    )

    dlc.executeDLC(Future.successful(oracleSig)).map(_ => succeed)
  }
}
