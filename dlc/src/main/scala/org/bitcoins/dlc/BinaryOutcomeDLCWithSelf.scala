package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  ECPublicKey,
  Schnorr,
  SchnorrDigitalSignature
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  MultiSignatureWithTimeoutScriptPubKey,
  P2PKHScriptPubKey,
  ScriptPubKey
}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfo
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

// Two selfs: Local, Remote
// Two outcomes: Win, Lose
case class BinaryOutcomeDLCWithSelf(
    outcomeWin: String,
    outcomeLose: String,
    oraclePubKey: ECPublicKey,
    preCommittedR: ECPublicKey,
    fundingLocalPrivKey: ECPrivateKey,
    fundingRemotePrivKey: ECPrivateKey,
    cetLocalPrivKey: ECPrivateKey,
    cetRemotePrivKey: ECPrivateKey,
    finalLocalPrivKey: ECPrivateKey,
    finalRemotePrivKey: ECPrivateKey,
    localInput: CurrencyUnit,
    remoteInput: CurrencyUnit,
    localFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    remoteFundingUtxos: Vector[BitcoinUTXOSpendingInfo],
    localWinPayout: CurrencyUnit,
    remoteWinPayout: CurrencyUnit,
    localLosePayout: CurrencyUnit,
    remoteLosePayout: CurrencyUnit,
    timeout: Int,
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey,
    network: BitcoinNetwork)(implicit ec: ExecutionContext) {

  val messageWin: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip.bytes

  val messageLose: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip.bytes

  val sigPubKeyWin: ECPublicKey =
    Schnorr.computePubKey(messageWin, preCommittedR, oraclePubKey)

  val sigPubKeyLose: ECPublicKey =
    Schnorr.computePubKey(messageLose, preCommittedR, oraclePubKey)

  private val totalInput = localInput + remoteInput
  private val fundingUtxos = localFundingUtxos ++ remoteFundingUtxos

  val fundingLocalPubKey: ECPublicKey = fundingLocalPrivKey.publicKey
  val fundingRemotePubKey: ECPublicKey = fundingRemotePrivKey.publicKey

  val fundingSPK: MultiSignatureScriptPubKey = {
    MultiSignatureScriptPubKey(2,
                               Vector(fundingLocalPubKey, fundingRemotePubKey))
  }

  def createFundingTransaction: Future[Transaction] = {
    println("CHECKPOINT 0")
    val output: TransactionOutput =
      TransactionOutput(totalInput + (Satoshis.one * 350), fundingSPK)

    val outputs: Vector[TransactionOutput] = Vector(output)
    val txBuilderF: Future[BitcoinTxBuilder] =
      BitcoinTxBuilder(outputs, fundingUtxos, feeRate, changeSPK, network)
    println("CHECKPOINT 1")
    txBuilderF.flatMap(_.sign)
  }

  def toLocalSPK(
      sigPubKey: ECPublicKey): MultiSignatureWithTimeoutScriptPubKey = {
    MultiSignatureWithTimeoutScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetLocalPrivKey.publicKey, sigPubKey),
      timeout = timeout,
      timeoutPubKey = cetRemotePrivKey.publicKey)
  }

  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
    println("CHECKPOINT 4")
    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetLocalPrivKey.publicKey, sigPubKey),
      timeout = timeout,
      timeoutPubKey = cetRemotePrivKey.publicKey)

    val toLocal: TransactionOutput =
      TransactionOutput(localPayout, toLocalSPK)
    val toRemote: TransactionOutput =
      TransactionOutput(remotePayout,
                        P2PKHScriptPubKey(cetRemotePrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       changeSPK,
                       network)

    println("CHECKPOINT 5")
    txBuilderF.flatMap(_.sign)
  }

  def createCETRemote(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetRemotePrivKey.publicKey, sigPubKey),
      timeout = timeout,
      timeoutPubKey = cetLocalPrivKey.publicKey)

    val toLocal: TransactionOutput =
      TransactionOutput(remotePayout, toLocalSPK)
    val toRemote: TransactionOutput =
      TransactionOutput(localPayout,
                        P2PKHScriptPubKey(cetLocalPrivKey.publicKey))

    val outputs: Vector[TransactionOutput] = Vector(toLocal, toRemote)
    val txBuilderF =
      BitcoinTxBuilder(outputs,
                       Vector(fundingSpendingInfo),
                       feeRate,
                       changeSPK,
                       network)

    txBuilderF.flatMap(_.sign)
  }

  def createCETWinLocal(
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseLocal(
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETLocal(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def createCETWinRemote(
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKeyWin,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localWinPayout,
      remotePayout = remoteWinPayout
    )
  }

  def createCETLoseRemote(
      fundingSpendingInfo: BitcoinUTXOSpendingInfo): Future[Transaction] = {
    createCETRemote(
      sigPubKey = sigPubKeyLose,
      fundingSpendingInfo = fundingSpendingInfo,
      localPayout = localLosePayout,
      remotePayout = remoteLosePayout
    )
  }

  def executeDLC(
      oracleSigF: Future[SchnorrDigitalSignature]): Future[Transaction] = {
    createFundingTransaction.flatMap { fundingTx =>
      println("CHECKPOINT 2")
      println(s"Funding Transaction: ${fundingTx.hex}\n")

      val fundingTxId = fundingTx.txIdBE
      val fundingSpendingInfo = BitcoinUTXOSpendingInfo(
        outPoint = TransactionOutPoint(fundingTxId, UInt32.zero),
        output = fundingTx.outputs.head,
        signers = Vector(fundingLocalPrivKey, fundingRemotePrivKey),
        redeemScriptOpt = Some(fundingSPK),
        scriptWitnessOpt = None,
        hashType = HashType.sigHashAll
      )

      println("CHECKPOINT 3")
      val cetWinLocalF = createCETWinLocal(fundingSpendingInfo)
      val cetLoseLocalF = createCETLoseLocal(fundingSpendingInfo)
      val cetWinRemoteF = createCETWinRemote(fundingSpendingInfo)
      val cetLoseRemoteF = createCETLoseRemote(fundingSpendingInfo)

      cetWinLocalF.foreach(cet => println(s"CET Win Local: ${cet.hex}\n"))
      cetLoseLocalF.foreach(cet => println(s"CET Lose Local: ${cet.hex}\n"))
      cetWinRemoteF.foreach(cet => println(s"CET Win Remote: ${cet.hex}\n"))
      cetLoseRemoteF.foreach(cet => println(s"CET Lose Remote: ${cet.hex}\n"))

      // Publish funding tx

      oracleSigF.flatMap { oracleSig =>
        val cetLocalF =
          if (Schnorr.verify(messageWin, oracleSig, oraclePubKey)) {
            cetWinLocalF
          } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
            cetLoseLocalF
          } else {
            Future.failed(???)
          }

        cetLocalF.flatMap { cet =>
          val cetSpendingInfo = BitcoinUTXOSpendingInfo(
            TransactionOutPoint(cet.txIdBE, UInt32.zero),
            cet.outputs.head,
            Vector(cetLocalPrivKey, ECPrivateKey(oracleSig.s)),
            Some(toLocalSPK(sigPubKeyWin)),
            None,
            HashType.sigHashAll
          )

          val txBuilder = BitcoinTxBuilder(
            Vector(
              TransactionOutput(
                localWinPayout - (Satoshis.one * 232),
                P2PKHScriptPubKey(finalLocalPrivKey.publicKey))),
            Vector(cetSpendingInfo),
            feeRate,
            changeSPK,
            network
          )

          val spendingTxF = txBuilder.flatMap(_.sign)

          spendingTxF.foreach(tx => println(s"Closing Tx: ${tx.hex}"))

          // Publish tx

          spendingTxF
        }
      }
    }
  }
}
