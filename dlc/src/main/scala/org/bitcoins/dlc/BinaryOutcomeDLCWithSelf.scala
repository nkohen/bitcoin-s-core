package org.bitcoins.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  ECPublicKey,
  Schnorr,
  SchnorrDigitalSignature
}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{
  MultiSignatureScriptPubKey,
  MultiSignatureWithTimeoutScriptPubKey,
  P2PKHScriptPubKey,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
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
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).bytes

  val messageLose: ByteVector =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).bytes

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

    val output: TransactionOutput =
      TransactionOutput(totalInput, P2WSHWitnessSPKV0(fundingSPK))

    val outputs: Vector[TransactionOutput] = Vector(output)
    val txBuilderF: Future[BitcoinTxBuilder] =
      BitcoinTxBuilder(outputs, fundingUtxos, feeRate, changeSPK, network)

    txBuilderF.flatMap(_.sign)
  }

  def createCETLocal(
      sigPubKey: ECPublicKey,
      fundingSpendingInfo: BitcoinUTXOSpendingInfo,
      localPayout: CurrencyUnit,
      remotePayout: CurrencyUnit): Future[Transaction] = {
    val toLocalSPK = MultiSignatureWithTimeoutScriptPubKey(
      requiredSigs = 2,
      pubKeys = Vector(cetLocalPrivKey.publicKey, sigPubKey),
      timeout = timeout,
      timeoutPubKey = cetRemotePrivKey.publicKey)

    val toLocal: TransactionOutput =
      TransactionOutput(localPayout, P2WSHWitnessSPKV0(toLocalSPK))
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
      TransactionOutput(remotePayout, P2WSHWitnessSPKV0(toLocalSPK))
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
      val fundingTxId = fundingTx.txIdBE
      val fundingSpendingInfo = BitcoinUTXOSpendingInfo(
        outPoint = TransactionOutPoint(fundingTxId, UInt32.zero),
        output = fundingTx.outputs.head,
        signers = Vector(fundingLocalPrivKey, fundingRemotePrivKey),
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(fundingSPK)),
        hashType = HashType.sigHashAll
      )

      val cetWinLocalF = createCETWinLocal(fundingSpendingInfo)
      val cetLoseLocalF = createCETLoseLocal(fundingSpendingInfo)
      val cetWinRemoteF = createCETWinRemote(fundingSpendingInfo)
      val cetLoseRemoteF = createCETLoseRemote(fundingSpendingInfo)

      // Publish funding tx

      oracleSigF.flatMap { oracleSig =>
        if (Schnorr.verify(messageWin, oracleSig, oraclePubKey)) {
          cetWinLocalF.map { cet =>
            val cetSpendingInfo = BitcoinUTXOSpendingInfo(
              TransactionOutPoint(cet.txIdBE, UInt32.zero),
              cet.outputs.head,
              Vector(cetLocalPrivKey, ECPrivateKey(oracleSig.s)),
              None,
              ???,
              HashType.sigHashAll)

            ???
          }
        } else if (Schnorr.verify(messageLose, oracleSig, oraclePubKey)) {
          cetLoseLocalF.map { cet =>
            ???
          }
        } else {
          Future.failed(???)
        }
      }
    }
  }
}
