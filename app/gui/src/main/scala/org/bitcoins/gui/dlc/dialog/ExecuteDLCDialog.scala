package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ExecuteDLC
import org.bitcoins.crypto.SchnorrDigitalSignature
import scalafx.scene.Node
import scalafx.scene.control.TextField
import scodec.bits.ByteVector

object ExecuteDLCDialog
    extends DLCDialog[ExecuteDLC](
      "DLC Close",
      "Enter DLC execution info",
      Vector(DLCDialog.dlcContractIdStr -> new TextField(),
             DLCDialog.dlcOracleSigStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, Node]): ExecuteDLC = {
    val contractId = readStringFromNode(inputs(dlcContractIdStr))
    val oracleSigsStr = readStringFromNode(inputs(dlcOracleSigStr))

    val oracleSigs = oracleSigsStr.split(",").map { str =>
      SchnorrDigitalSignature.fromHex(str.trim)
    }

    ExecuteDLC(ByteVector.fromValidHex(contractId),
               oracleSigs.toVector,
               noBroadcast = false)
  }
}
