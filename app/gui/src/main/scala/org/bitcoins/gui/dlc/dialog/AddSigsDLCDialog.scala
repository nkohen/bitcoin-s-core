package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.AddDLCSigs
import org.bitcoins.core.protocol.tlv.DLCSignTLV

object AddSigsDLCDialog
    extends DLCDialog[AddDLCSigs](
      "Sign DLC",
      "Enter DLC signatures message",
      Vector(DLCDialog.dlcSigStr -> DLCDialog.textArea())) {

  import DLCDialog._

  override def constructFromInput(inputs: Map[String, String]): AddDLCSigs = {
    val sign = DLCSignTLV(dlcSigStr)
    AddDLCSigs(sign)
  }
}
