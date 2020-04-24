package org.bitcoins.wallet.db

import org.bitcoins.db.{DbManagement, JdbcProfileComponent}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AccountDAO,
  AddressDAO,
  IncomingTransactionDAO,
  OutgoingTransactionDAO,
  SpendingInfoDAO,
  TransactionDAO
}

import scala.concurrent.ExecutionContext

trait WalletDbManagement extends DbManagement {
  _: JdbcProfileComponent[WalletAppConfig] =>

  import profile.api._

  def ec: ExecutionContext

  private lazy val accountTable: TableQuery[Table[_]] = {
    AccountDAO()(ec, appConfig).table
      .asInstanceOf[TableQuery[Table[_]]]
  }
  private lazy val addressTable: TableQuery[Table[_]] = {
    AddressDAO()(ec, appConfig).table
      .asInstanceOf[TableQuery[Table[_]]]
  }
  private lazy val utxoTable: TableQuery[Table[_]] = {
    SpendingInfoDAO()(ec, appConfig).table
      .asInstanceOf[TableQuery[Table[_]]]
  }
  private lazy val txTable: TableQuery[Table[_]] = {
    TransactionDAO()(ec, appConfig).table
      .asInstanceOf[TableQuery[Table[_]]]
  }
  private lazy val incomingTxTable: TableQuery[Table[_]] = {
    IncomingTransactionDAO()(ec, appConfig).table
      .asInstanceOf[TableQuery[Table[_]]]
  }
  private lazy val outgoingTxTable: TableQuery[Table[_]] = {
    OutgoingTransactionDAO()(ec, appConfig).table
      .asInstanceOf[TableQuery[Table[_]]]
  }

  override lazy val allTables: List[TableQuery[Table[_]]] = {
    List(accountTable,
         addressTable,
         utxoTable,
         txTable,
         incomingTxTable,
         outgoingTxTable)
  }

}
