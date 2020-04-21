package org.bitcoins.gui.dlc.dialog

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{SchnorrDigitalSignature, SchnorrPublicKey}
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2PKHScriptPubKey}
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{CryptoUtil, NumberUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.P2PKHSpendingInfo
import org.bitcoins.dlc.DLCClient
import scalafx.Includes._
import scalafx.geometry.Insets
import scalafx.scene.control.{ButtonType, Dialog, Label, TextField}
import scalafx.scene.layout.GridPane
import scalafx.stage.Window
import scodec.bits.ByteVector

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt

object PunishOracleDialog {

  def showAndWait(parentWindow: Window): Option[Transaction] = {
    val dialog = new Dialog[Option[Transaction]]() {
      initOwner(parentWindow)
      title = "Punish Equivocating Oracle"
      headerText =
        "Enter Contradicting Oracle Signatures and Punishment Address"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)

    val oraclePubKeyTF = new TextField()
    val oracleSig1TF = new TextField()
    val msg1TF = new TextField()
    val oracleSig2TF = new TextField()
    val msg2TF = new TextField()
    val oracleOutPointTF = new TextField()
    val sweepAddressTF = new TextField()

    dialog.dialogPane().content = new GridPane {
      hgap = 10
      vgap = 10
      padding = Insets(20, 100, 10, 10)

      var nextRow: Int = 0
      def addRow(label: String, textField: TextField): Unit = {
        add(new Label(label), 0, nextRow)
        add(textField, 1, nextRow)
        nextRow += 1
      }

      addRow("Oracle Public Key", oraclePubKeyTF)
      addRow("Oracle Signature 1", oracleSig1TF)
      addRow("Message 1", msg1TF)
      addRow("Oracle Signature 2", oracleSig2TF)
      addRow("Message 2", msg2TF)
      addRow("Oracle Punishment OutPoint", oracleOutPointTF)
      addRow("Sweep Address", sweepAddressTF)
    }

    // Enable/Disable OK button depending on whether all data was entered.
    val okButton = dialog.dialogPane().lookupButton(ButtonType.OK)
    // Simple validation that sufficient data was entered
    okButton.disable <== (oraclePubKeyTF.text.isEmpty ||
    oracleSig1TF.text.isEmpty ||
    msg1TF.text.isEmpty ||
    oracleSig2TF.text.isEmpty ||
    msg2TF.text.isEmpty ||
    oracleOutPointTF.text.isEmpty ||
    sweepAddressTF.text.isEmpty)

    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        val pubKey = SchnorrPublicKey(oraclePubKeyTF.text())
        val sig1 = SchnorrDigitalSignature(oracleSig1TF.text())
        val sig2 = SchnorrDigitalSignature(oracleSig2TF.text())

        // s1 = nonce + e1*privKey
        val s1 = sig1.sig
        // s2 = nonce + e2*privKey
        val s2 = sig2.sig

        // When signing a message you actually sign SHA256_challenge(Rx || pubKey || message)
        val bytesToHash1 = sig1.rx.bytes ++ pubKey.bytes ++ ByteVector
          .fromHex(msg1TF.text())
          .get
        val e1Bytes = CryptoUtil.sha256SchnorrChallenge(bytesToHash1).bytes

        val bytesToHash2 = sig2.rx.bytes ++ pubKey.bytes ++ ByteVector
          .fromHex(msg2TF.text())
          .get
        val e2Bytes = CryptoUtil.sha256SchnorrChallenge(bytesToHash2).bytes

        val e1 = NumberUtil.uintToFieldElement(e1Bytes)
        val e2 = NumberUtil.uintToFieldElement(e2Bytes)

        // s1 - s2 = nonce + e1*privKey - (nonce + e2*privKey) = privKey*(e1-e2)
        // => privKey = (s1 - s2) * modInverse(e1 - e2)
        val privNum = s1.subtract(s2).multInv(e1.subtract(e2))

        val privKey = privNum.toPrivateKey

        val outPoint = TransactionOutPoint(oracleOutPointTF.text())

        val spendingInfo = P2PKHSpendingInfo(
          outPoint,
          Bitcoins.one,
          P2PKHScriptPubKey(pubKey.publicKey),
          privKey,
          HashType.sigHashAll)

        val sweepAddress = BitcoinAddress(sweepAddressTF.text()).get

        import scala.concurrent.ExecutionContext.Implicits.global
        val txF = BitcoinTxBuilder(
          Vector(TransactionOutput(Bitcoins.one, sweepAddress.scriptPubKey)),
          Vector(spendingInfo),
          SatoshisPerVirtualByte(Satoshis(100)),
          EmptyScriptPubKey,
          RegTest
        ).flatMap(DLCClient.subtractFeeAndSign)
        val tx = Await.result(txF, 5.seconds)

        Some(tx)
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(Some(tx: Transaction)) => Some(tx)
      case Some(_) | None              => None
    }
  }
}
