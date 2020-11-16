package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.SignDLC
import org.bitcoins.core.protocol.tlv.DLCAcceptTLV

object SignDLCDialog
    extends DLCDialog[SignDLC](
      "Sign DLC",
      "Enter DLC accept message",
      Vector(DLCDialog.dlcAcceptStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): SignDLC = {
    val accept = DLCAcceptTLV(dlcAcceptStr)
    SignDLC(accept)
  }
}
