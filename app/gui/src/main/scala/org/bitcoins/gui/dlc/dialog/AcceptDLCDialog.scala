package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.AcceptDLCOffer
import org.bitcoins.core.protocol.tlv._

object AcceptDLCDialog
    extends DLCDialog[AcceptDLCOffer](
      "Accept DLC Offer",
      "Enter DLC offer to accept",
      Vector(DLCDialog.dlcOfferStr -> DLCDialog.textArea())) {
  import DLCDialog._

  override def constructFromInput(
      inputs: Map[String, String]): AcceptDLCOffer = {
    val offer = LnMessageFactory(DLCOfferTLV).fromHex(dlcOfferStr)

    AcceptDLCOffer(offer)
  }
}
