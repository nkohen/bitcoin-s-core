package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.BroadcastDLCFundingTx
import scalafx.scene.control.TextField
import scodec.bits.ByteVector

object GetFundingDLCDialog
    extends DLCDialog[BroadcastDLCFundingTx](
      "DLC Funding Transaction",
      "Enter DLC contract ID",
      Vector(DLCDialog.dlcContractIdStr -> new TextField())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): BroadcastDLCFundingTx = {
    val contractId = ByteVector.fromValidHex(inputs(dlcContractIdStr))
    BroadcastDLCFundingTx(contractId)
  }
}
