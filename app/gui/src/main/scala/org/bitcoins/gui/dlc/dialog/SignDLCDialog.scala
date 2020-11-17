package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.SignDLC
import org.bitcoins.core.protocol.tlv._

object SignDLCDialog
    extends DLCDialog[SignDLC](
      "Sign DLC",
      "Enter DLC accept message",
      Vector(DLCDialog.dlcAcceptStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): SignDLC = {
    val accept = LnMessageFactory(DLCAcceptTLV).fromHex(dlcAcceptStr)
    SignDLC(accept)
  }
}
