package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ExecuteDLC
import org.bitcoins.crypto.SchnorrDigitalSignature
import scalafx.scene.control.TextField
import scodec.bits.ByteVector

object ExecuteDLCDialog
    extends DLCDialog[ExecuteDLC](
      "DLC Close",
      "Enter DLC closing info",
      Vector(DLCDialog.dlcContractIdStr -> new TextField(),
             DLCDialog.dlcOracleSigStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): ExecuteDLC = {
    val contractId = inputs(dlcContractIdStr)
    val oracleSig = SchnorrDigitalSignature(inputs(dlcOracleSigStr))
    ExecuteDLC(ByteVector.fromValidHex(contractId),
               Vector(oracleSig),
               noBroadcast = false)
  }
}
