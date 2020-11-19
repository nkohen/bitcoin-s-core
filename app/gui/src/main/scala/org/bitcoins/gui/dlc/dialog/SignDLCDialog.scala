package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.tlv._
import scalafx.scene.Node

object SignDLCDialog
    extends DLCDialog[SignDLCCliCommand](
      "Sign DLC",
      "Enter DLC accept message",
      Vector(
        DLCDialog.dlcAcceptStr -> DLCDialog
          .textArea(),
        "Open Accept from File" ->
          DLCDialog.fileChooserButton(file =>
            DLCDialog.acceptDLCFile = Some(file))
      ),
      Vector(DLCDialog.dlcAcceptStr, DLCDialog.dlcAcceptFileStr)) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, Node]): SignDLCCliCommand = {
    acceptDLCFile match {
      case Some(file) =>
        acceptDLCFile = None // reset
        SignDLCFromFile(file.toPath)
      case None =>
        val acceptHex = readStringFromNode(inputs(dlcAcceptStr))

        val accept = LnMessageFactory(DLCAcceptTLV).fromHex(acceptHex)
        SignDLC(accept)
    }
  }
}
