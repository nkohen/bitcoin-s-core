package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.ExecuteDLCRefund
import scalafx.scene.control.TextField
import scodec.bits.ByteVector

object RefundDLCDialog
    extends DLCDialog[ExecuteDLCRefund](
      "DLC Refund",
      "Enter DLC contract ID",
      Vector(DLCDialog.dlcContractIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): ExecuteDLCRefund = {
    val contractId = ByteVector.fromValidHex(inputs(dlcContractIdStr))
    ExecuteDLCRefund(contractId, noBroadcast = false)
  }
}
