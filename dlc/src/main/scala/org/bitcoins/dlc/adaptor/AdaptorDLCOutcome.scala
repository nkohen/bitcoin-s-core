package org.bitcoins.dlc.adaptor

import org.bitcoins.core.protocol.transaction.Transaction

sealed trait AdaptorDLCOutcome {
  def fundingTx: Transaction
}

case class ExecutedDLCOutcome(
    override val fundingTx: Transaction,
    cet: Transaction)
    extends AdaptorDLCOutcome

case class RefundDLCOutcome(
    override val fundingTx: Transaction,
    refundTx: Transaction)
    extends AdaptorDLCOutcome
