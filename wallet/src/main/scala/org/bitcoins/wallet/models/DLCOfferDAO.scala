package org.bitcoins.wallet.models

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.ContractInfo
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.db.{CRUD, SlickUtil}
import org.bitcoins.wallet.config._
import slick.lifted.{ForeignKeyQuery, PrimaryKey, ProvenShape}

import scala.concurrent.{ExecutionContext, Future}

case class DLCOfferDAO()(implicit
    val ec: ExecutionContext,
    override val appConfig: WalletAppConfig)
    extends CRUD[DLCOfferDb, Sha256DigestBE]
    with SlickUtil[DLCOfferDb, Sha256DigestBE] {
  private val mappers = new org.bitcoins.db.DbCommonsColumnMappers(profile)
  import mappers._
  import profile.api._

  override val table: TableQuery[DLCOfferTable] = TableQuery[DLCOfferTable]

  private lazy val dlcTable: slick.lifted.TableQuery[DLCDAO#DLCTable] = {
    DLCDAO().table
  }

  override def createAll(ts: Vector[DLCOfferDb]): Future[Vector[DLCOfferDb]] =
    createAllNoAutoInc(ts, safeDatabase)

  override protected def findByPrimaryKeys(
      ids: Vector[Sha256DigestBE]): Query[DLCOfferTable, DLCOfferDb, Seq] =
    table.filter(_.eventId.inSet(ids))

  override def findByPrimaryKey(
      id: Sha256DigestBE): Query[DLCOfferTable, DLCOfferDb, Seq] = {
    table
      .filter(_.eventId === id)
  }

  override def findAll(
      dlcs: Vector[DLCOfferDb]): Query[DLCOfferTable, DLCOfferDb, Seq] =
    findByPrimaryKeys(dlcs.map(_.eventId))

  def findByEventId(eventId: Sha256DigestBE): Future[Option[DLCOfferDb]] = {
    val q = table.filter(_.eventId === eventId)

    safeDatabase.run(q.result).map {
      case h +: Vector() =>
        Some(h)
      case Vector() =>
        None
      case dlcs: Vector[DLCOfferDb] =>
        throw new RuntimeException(
          s"More than one DLCOffer per eventId ($eventId), got: $dlcs")
    }
  }

  def findByEventId(eventId: Sha256Digest): Future[Option[DLCOfferDb]] =
    findByEventId(eventId.flip)

  class DLCOfferTable(tag: Tag)
      extends Table[DLCOfferDb](tag, "wallet_dlc_offers") {

    def eventId: Rep[Sha256DigestBE] = column("event_id", O.Unique)

    def oraclePubKey: Rep[SchnorrPublicKey] = column("oracle_pub_key")

    def oracleRValue: Rep[SchnorrNonce] = column("oracle_r_value")

    def contractInfo: Rep[ContractInfo] = column("contract_info")

    def contractMaturity: Rep[BlockTimeStamp] = column("contract_maturity")

    def contractTimeout: Rep[BlockTimeStamp] = column("contract_timeout")

    def fundingKey: Rep[ECPublicKey] = column("funding_key")

    def payoutAddress: Rep[BitcoinAddress] = column("payout_address")

    def totalCollateral: Rep[CurrencyUnit] = column("total_collateral")

    def feeRate: Rep[SatoshisPerVirtualByte] = column("fee_rate")

    def changeAddress: Rep[BitcoinAddress] = column("change_address")

    def * : ProvenShape[DLCOfferDb] =
      (eventId,
       oraclePubKey,
       oracleRValue,
       contractInfo,
       contractMaturity,
       contractTimeout,
       fundingKey,
       payoutAddress,
       totalCollateral,
       feeRate,
       changeAddress) <> (DLCOfferDb.tupled, DLCOfferDb.unapply)

    def primaryKey: PrimaryKey =
      primaryKey(name = "pk_dlc_offer", sourceColumns = eventId)

    def fk: ForeignKeyQuery[_, DLCDb] =
      foreignKey("fk_eventId",
                 sourceColumns = eventId,
                 targetTableQuery = dlcTable)(_.eventId)
  }
}