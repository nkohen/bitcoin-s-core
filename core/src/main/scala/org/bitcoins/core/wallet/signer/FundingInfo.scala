package org.bitcoins.core.wallet.signer

import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  WitnessTransaction
}
import org.bitcoins.core.wallet.utxo.NewSpendingInfo

/**
  * This meant to represent the class used to 'fund' an
  * unsigned [[org.bitcoins.core.protocol.transaction.Transaction Transaction]].
  * This is useful for when we have multiple [[org.bitcoins.core.config.NetworkParameters NetworkParameters]]
  * that each have their own transaction type. I.e. we should only be able to have
  * BitcoinTransactions paired with [[org.bitcoins.core.wallet.utxo.NewSpendingInfo.Any NewSpendingInfo]],
  * the same would apply for litecoin etc.
  */
sealed abstract class FundingInfo {

  /** The transaction we are funding with the utxos */
  def transaction: Transaction

  /** The utxos used to fund the tx */
  def utxos: Seq[NewSpendingInfo.Any]
}

sealed abstract class BitcoinFundingInfo extends FundingInfo {
  override def utxos: Seq[NewSpendingInfo.Any]
}

object BitcoinFundingInfo {
  private case class BitcoinFundingInfoImpl(
      transaction: Transaction,
      utxos: Seq[NewSpendingInfo.Any])
      extends BitcoinFundingInfo

  def apply(
      tx: BaseTransaction,
      utxos: Seq[NewSpendingInfo.Any]): BitcoinFundingInfo = {
    BitcoinFundingInfoImpl(tx, utxos)
  }

  def apply(
      wtx: WitnessTransaction,
      utxos: Seq[NewSpendingInfo.Any]): BitcoinFundingInfo = {
    BitcoinFundingInfoImpl(wtx, utxos)
  }
}
