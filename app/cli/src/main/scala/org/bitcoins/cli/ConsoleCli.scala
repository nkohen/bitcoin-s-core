package org.bitcoins.cli

import org.bitcoins.cli.CliCommand._
import org.bitcoins.cli.CliReaders._
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.crypto.Sha256DigestBE
import org.bitcoins.core.currency._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.transaction.{EmptyTransaction, Transaction}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage.{DLCAccept, DLCOffer, OracleInfo}
import org.bitcoins.picklers._
import scopt.OParser
import ujson.{Num, Str}
import upickle.{default => up}

import scala.collection.mutable
import scala.util.{Failure, Success, Try}

object ConsoleCli {

  def parser: OParser[Unit, Config] = {
    val builder = OParser.builder[Config]

    import builder._
    OParser.sequence(
      programName("bitcoin-s-cli"),
      opt[NetworkParameters]('n', "network")
        .action((np, conf) => conf.copy(network = Some(np)))
        .text("Select the active network."),
      opt[Unit]("debug")
        .action((_, conf) => conf.copy(debug = true))
        .text("Print debugging information"),
      cmd("getblockcount")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBlockCount))
        .text(s"Get the block height"),
      cmd("getfiltercount")
        .hidden()
        .action((_, conf) => conf.copy(command = GetFilterCount))
        .text(s"Get the number of filters"),
      cmd("getfilterheadercount")
        .hidden()
        .action((_, conf) => conf.copy(command = GetFilterHeaderCount))
        .text(s"Get the number of filter headers"),
      cmd("getbestblockhash")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBestBlockHash))
        .text(s"Get the best block hash"),
      cmd("rescan")
        .hidden()
        .action(
          (_, conf) =>
            conf.copy(
              command = Rescan(addressBatchSize = Option.empty,
                               startBlock = Option.empty,
                               endBlock = Option.empty,
                               force = false)))
        .text(s"Rescan UTXOs")
        .children(
          opt[Unit]("force")
            .optional()
            .action((_, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(force = true)
                case other => other
              })),
          opt[Int]("batch-size")
            .optional()
            .action((batchSize, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(addressBatchSize = Option(batchSize))
                case other => other
              })),
          opt[BlockStamp]("start")
            .optional()
            .action((start, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(startBlock = Option(start))
                case other => other
              })),
          opt[BlockStamp]("end")
            .optional()
            .action((end, conf) =>
              conf.copy(command = conf.command match {
                case rescan: Rescan =>
                  rescan.copy(endBlock = Option(end))
                case other => other
              }))
        ),
      cmd("getbalance")
        .hidden()
        .action((_, conf) => conf.copy(command = GetBalance))
        .text("Get the wallet balance"),
      cmd("createdlcoffer")
        .hidden()
        .action(
          (_, conf) =>
            conf.copy(
              command = CreateDLCOffer(0.bitcoins,
                                       null,
                                       Vector.empty,
                                       None,
                                       UInt32.zero,
                                       UInt32.zero,
                                       escaped = false)))
        .text("Creates a DLC offer that another party can accept")
        .children(
          opt[Bitcoins]("amount").required
            .action((amt, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(amount = amt)
                case other => other
              })),
          opt[OracleInfo]("oracleInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(oracleInfo = info)
                case other => other
              })),
          opt[Seq[Sha256DigestBE]]("contractInfo")
            .required()
            .action((info, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(contractInfo = info)
                case other => other
              })),
          opt[SatoshisPerVirtualByte]("feerate")
            .optional()
            .action((feeRate, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(feeRateOpt = Some(feeRate))
                case other => other
              })),
          opt[UInt32]("locktime")
            .required()
            .action((locktime, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(locktime = locktime)
                case other => other
              })),
          opt[UInt32]("refundlocktime")
            .required()
            .action((refundLT, conf) =>
              conf.copy(command = conf.command match {
                case offer: CreateDLCOffer =>
                  offer.copy(refundLT = refundLT)
                case other => other
              })),
          opt[Boolean]("escaped")
            .action((escaped, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(escaped = escaped)
                case other => other
              }))
        ),
      cmd("acceptdlcoffer")
        .hidden()
        .action((_, conf) =>
          conf.copy(
            command = AcceptDLCOffer(null, 0.bitcoins, escaped = false)))
        .text("Accepts a DLC offer given from another party")
        .children(
          opt[DLCOffer]("offer").required
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(offer = offer)
                case other => other
              })),
          opt[Bitcoins]("amount").required
            .action((amt, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(amount = amt)
                case other => other
              })),
          opt[Boolean]("escaped")
            .action((escaped, conf) =>
              conf.copy(command = conf.command match {
                case accept: AcceptDLCOffer =>
                  accept.copy(escaped = escaped)
                case other => other
              }))
        ),
      cmd("signdlc")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = SignDLC(null, null, escaped = false)))
        .text("Signs a DLC")
        .children(
          opt[DLCOffer]("offer").required
            .action((offer, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(offer = offer)
                case other => other
              })),
          opt[DLCAccept]("accept").required
            .action((accept, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(accept = accept)
                case other => other
              })),
          opt[Boolean]("escaped")
            .action((escaped, conf) =>
              conf.copy(command = conf.command match {
                case signDLC: SignDLC =>
                  signDLC.copy(escaped = escaped)
                case other => other
              }))
        ),
      cmd("getnewaddress")
        .hidden()
        .action((_, conf) => conf.copy(command = GetNewAddress))
        .text("Get a new address"),
      cmd("sendtoaddress")
        .hidden()
        .action(
          // TODO how to handle null here?
          (_, conf) => conf.copy(command = SendToAddress(null, 0.bitcoin)))
        .text("Send money to the given address")
        .children(
          opt[BitcoinAddress]("address")
            .required()
            .action((addr, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(destination = addr)
                case other => other
              })),
          opt[Bitcoins]("amount")
            .required()
            .action((btc, conf) =>
              conf.copy(command = conf.command match {
                case send: SendToAddress =>
                  send.copy(amount = btc)
                case other => other
              }))
        ),
      cmd("getpeers")
        .hidden()
        .action((_, conf) => conf.copy(command = GetPeers))
        .text(s"List the connected peers"),
      cmd("combinepsbts")
        .hidden()
        .action((_, conf) => conf.copy(command = CombinePSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          opt[Seq[PSBT]]("psbts")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case combinePSBTs: CombinePSBTs =>
                  combinePSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("joinpsbts")
        .hidden()
        .action((_, conf) => conf.copy(command = JoinPSBTs(Seq.empty)))
        .text("Combines all the given PSBTs")
        .children(
          opt[Seq[PSBT]]("psbts")
            .required()
            .action((seq, conf) =>
              conf.copy(command = conf.command match {
                case joinPSBTs: JoinPSBTs =>
                  joinPSBTs.copy(psbts = seq)
                case other => other
              }))
        ),
      cmd("finalizepsbt")
        .hidden()
        .action((_, conf) => conf.copy(command = FinalizePSBT(PSBT.empty)))
        .text("Finalizes the given PSBT if it can")
        .children(
          opt[PSBT]("psbt")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case finalizePSBT: FinalizePSBT =>
                  finalizePSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("extractfrompsbt")
        .hidden()
        .action((_, conf) => conf.copy(command = ExtractFromPSBT(PSBT.empty)))
        .text("Extracts a transaction from the given PSBT if it can")
        .children(
          opt[PSBT]("psbt")
            .required()
            .action((psbt, conf) =>
              conf.copy(command = conf.command match {
                case extractFromPSBT: ExtractFromPSBT =>
                  extractFromPSBT.copy(psbt = psbt)
                case other => other
              }))
        ),
      cmd("converttopsbt")
        .hidden()
        .action((_, conf) =>
          conf.copy(command = ConvertToPSBT(EmptyTransaction)))
        .text("Creates an empty psbt from the given transaction")
        .children(
          opt[Transaction]("unsignedTx")
            .required()
            .action((tx, conf) =>
              conf.copy(command = conf.command match {
                case convertToPSBT: ConvertToPSBT =>
                  convertToPSBT.copy(transaction = tx)
                case other => other
              }))
        ),
      help('h', "help").text("Display this help message and exit"),
      arg[String]("<cmd>")
        .optional()
        .text(
          "The command and arguments to be executed. Try bitcoin-s-cli help for a list of all commands"),
      checkConfig {
        case Config(NoCommand, _, _) =>
          failure("You need to provide a command!")
        case _ => success
      }
    )
  }

  def exec(args: String*)(port: Int): Try[String] = {
    val config = OParser.parse(parser, args.toVector, Config()) match {
      case None       => sys.exit(1)
      case Some(conf) => conf
    }

    import System.err.{println => printerr}

    /** Prints the given message to stderr if debug is set */
    def debug(message: Any): Unit = {
      if (config.debug) {
        printerr(s"DEBUG: $message")
      }
    }

    /** Prints the given message to stderr and exist */
    def error[T](message: String): Failure[T] = {
      Failure(new RuntimeException(message))
    }

    val requestParam: RequestParam = config.command match {
      // DLC
      case CreateDLCOffer(amount,
                          oracleInfo,
                          contractInfo,
                          feeRateOpt,
                          locktime,
                          refundLT,
                          escaped) =>
        RequestParam(
          "createdlcoffer",
          Seq(
            up.writeJs(amount),
            up.writeJs(oracleInfo),
            up.writeJs(contractInfo),
            up.writeJs(feeRateOpt.getOrElse(SatoshisPerVirtualByte.one)),
            up.writeJs(locktime),
            up.writeJs(refundLT),
            up.writeJs(escaped)
          )
        )
      case AcceptDLCOffer(offer, amount, escaped) =>
        RequestParam(
          "acceptdlcoffer",
          Seq(up.writeJs(offer), up.writeJs(amount), up.writeJs(escaped)))
      case SignDLC(offer, accept, escaped) =>
        RequestParam(
          "signdlc",
          Seq(up.writeJs(offer), up.writeJs(accept), up.writeJs(escaped)))
      case GetBalance =>
        RequestParam("getbalance")
      case GetNewAddress =>
        RequestParam("getnewaddress")
      case Rescan(addressBatchSize, startBlock, endBlock, force) =>
        RequestParam("rescan",
                     Seq(up.writeJs(addressBatchSize),
                         up.writeJs(startBlock),
                         up.writeJs(endBlock),
                         up.writeJs(force)))

      case SendToAddress(address, bitcoins) =>
        RequestParam("sendtoaddress",
                     Seq(up.writeJs(address), up.writeJs(bitcoins)))
      // height
      case GetBlockCount => RequestParam("getblockcount")
      // filter count
      case GetFilterCount => RequestParam("getfiltercount")
      // filter header count
      case GetFilterHeaderCount => RequestParam("getfilterheadercount")
      // besthash
      case GetBestBlockHash => RequestParam("getbestblockhash")
      // peers
      case GetPeers => RequestParam("getpeers")
      // PSBTs
      case CombinePSBTs(psbts) =>
        RequestParam("combinepsbts", Seq(up.writeJs(psbts)))
      case JoinPSBTs(psbts) =>
        RequestParam("joinpsbts", Seq(up.writeJs(psbts)))
      case FinalizePSBT(psbt) =>
        RequestParam("finalizepsbt", Seq(up.writeJs(psbt)))
      case ExtractFromPSBT(psbt) =>
        RequestParam("extractfrompsbt", Seq(up.writeJs(psbt)))
      case ConvertToPSBT(tx) =>
        RequestParam("converttopsbt", Seq(up.writeJs(tx)))

      case NoCommand => ???
    }

    Try {
      import com.softwaremill.sttp._
      implicit val backend = HttpURLConnectionBackend()
      val request =
        sttp
          .post(uri"http://$host:$port/")
          .contentType("application/json")
          .body({
            val uuid = java.util.UUID.randomUUID.toString
            val paramsWithID: Map[String, ujson.Value] = requestParam.toJsonMap + ("id" -> up
              .writeJs(uuid))
            up.write(paramsWithID)
          })
      debug(s"HTTP request: $request")
      val response = request.send()

      debug(s"HTTP response:")
      debug(response)

      // in order to mimic Bitcoin Core we always send
      // an object looking like {"result": ..., "error": ...}
      val rawBody = response.body match {
        case Left(err)       => err
        case Right(response) => response
      }

      val js = ujson.read(rawBody)
      val jsObjT =
        Try(js.obj).transform[mutable.LinkedHashMap[String, ujson.Value]](
          Success(_),
          _ => error(s"Response was not a JSON object! Got: $rawBody"))

      /** Gets the given key from jsObj if it exists
        * and is not null */
      def getKey(key: String): Option[ujson.Value] = {
        jsObjT.toOption.flatMap(_.get(key).flatMap(result =>
          if (result.isNull) None else Some(result)))
      }

      /** Converts a `ujson.Value` to String, making an
        * effort to avoid preceding and trailing `"`s */
      def jsValueToString(value: ujson.Value) = value match {
        case Str(string)             => string
        case Num(num) if num.isWhole => num.toLong.toString
        case Num(num)                => num.toString
        case rest: ujson.Value       => rest.toString()
      }

      (getKey("result"), getKey("error")) match {
        case (Some(result), None) =>
          Success(jsValueToString(result))
        case (None, Some(err)) =>
          val msg = jsValueToString(err)
          error(msg)
        case (None, None) | (Some(_), Some(_)) =>
          error(s"Got unexpected response: $rawBody")
      }
    }.flatten
  }

  // TODO make this dynamic
  def defaultPort = 9999
  def host = "localhost"

  case class RequestParam(
      method: String,
      params: Seq[ujson.Value.Value] = Nil) {

    lazy val toJsonMap: Map[String, ujson.Value] = {
      Map("method" -> method, "params" -> params)
    }
  }
}

case class Config(
    command: CliCommand = CliCommand.NoCommand,
    network: Option[NetworkParameters] = None,
    debug: Boolean = false
)

sealed abstract class CliCommand

object CliCommand {
  case object NoCommand extends CliCommand

  // DLC
  case class CreateDLCOffer(
      amount: Bitcoins,
      oracleInfo: OracleInfo,
      contractInfo: Seq[Sha256DigestBE],
      feeRateOpt: Option[SatoshisPerVirtualByte],
      locktime: UInt32,
      refundLT: UInt32,
      escaped: Boolean)
      extends CliCommand

  case class AcceptDLCOffer(offer: DLCOffer, amount: Bitcoins, escaped: Boolean)
      extends CliCommand

  case class SignDLC(offer: DLCOffer, accept: DLCAccept, escaped: Boolean)
      extends CliCommand

  // Wallet
  case class SendToAddress(destination: BitcoinAddress, amount: Bitcoins)
      extends CliCommand
  case object GetNewAddress extends CliCommand
  case object GetBalance extends CliCommand

  // Node
  case object GetPeers extends CliCommand

  // Chain
  case object GetBestBlockHash extends CliCommand
  case object GetBlockCount extends CliCommand
  case object GetFilterCount extends CliCommand
  case object GetFilterHeaderCount extends CliCommand
  case class Rescan(
      addressBatchSize: Option[Int],
      startBlock: Option[BlockStamp],
      endBlock: Option[BlockStamp],
      force: Boolean)
      extends CliCommand

  // PSBT
  case class CombinePSBTs(psbts: Seq[PSBT]) extends CliCommand
  case class JoinPSBTs(psbts: Seq[PSBT]) extends CliCommand
  case class FinalizePSBT(psbt: PSBT) extends CliCommand
  case class ExtractFromPSBT(psbt: PSBT) extends CliCommand
  case class ConvertToPSBT(transaction: Transaction) extends CliCommand
}

object Demo {
  import org.bitcoins.core.crypto._
  import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
  import org.bitcoins.core.util.{CryptoUtil, FutureUtil}
  import org.bitcoins.core.wallet.utxo.P2WPKHV0SpendingInfo
  import org.bitcoins.dlc._
  import org.bitcoins.dlc.DLCMessage._
  import scodec.bits.ByteVector
  import org.bitcoins.dlc.testgen.{
    SerializedDLCTestVectorSerializers,
    SerializedSegwitSpendingInfo
  }
  import play.api.libs.json.{Json, Reads}

  import scala.concurrent.{ExecutionContext, Future, Promise}

  /*
  import org.bitcoins.cli.{ConsoleCli, Demo}
  import org.bitcoins.core.protocol.transaction.Transaction
   */

  def setupOracle(): (
      OracleInfo,
      Sha256DigestBE,
      Sha256DigestBE,
      SchnorrDigitalSignature,
      SchnorrDigitalSignature) = {
    val oraclePrivKey = ECPrivateKey.freshPrivateKey
    val oraclePubKey = oraclePrivKey.publicKey

    val oracleKValue = SchnorrNonce.freshNonce
    val oracleRValue = oracleKValue.publicKey

    val oracleInfo = OracleInfo(oraclePubKey, oracleRValue)

    val strA = "A"
    val hashA = CryptoUtil.sha256(ByteVector(strA.getBytes)).flip
    val sigA = Schnorr.signWithNonce(hashA.bytes, oraclePrivKey, oracleKValue)

    val strB = "B"
    val hashB = CryptoUtil.sha256(ByteVector(strB.getBytes)).flip
    val sigB = Schnorr.signWithNonce(hashB.bytes, oraclePrivKey, oracleKValue)

    println()
    println(s"Oracle Info: ${oracleInfo.hex}")
    println(s"Hash A: ${hashA.hex}")
    println(s"Hash B: ${hashB.hex}")
    println(s"Sig A: ${sigA.hex}")
    println(s"Sig B: ${sigB.hex}")
    println()

    (oracleInfo, hashA, hashB, sigA, sigB)
  }

  def setupFromOracleInfo(
      oracleInfoStr: String,
      hashAStr: String,
      hashBStr: String,
      sigAStr: String,
      sigBStr: String): (
      OracleInfo,
      Sha256DigestBE,
      Sha256DigestBE,
      SchnorrDigitalSignature,
      SchnorrDigitalSignature) = {
    (OracleInfo.fromHex(oracleInfoStr),
     Sha256DigestBE.fromHex(hashAStr),
     Sha256DigestBE.fromHex(hashBStr),
     SchnorrDigitalSignature.fromHex(sigAStr),
     SchnorrDigitalSignature.fromHex(sigBStr))
  }

  def parseKeyAndUTXOs(privKeyStr: String, fundingUTXOsStr: String): (
      ExtPrivateKey,
      Vector[P2WPKHV0SpendingInfo]) = {
    implicit val reads: Reads[SerializedSegwitSpendingInfo] =
      SerializedDLCTestVectorSerializers.serializedSegwitSpendingInfoReads

    val privKey = ExtPrivateKey.fromString(privKeyStr).get
    val fundingUTXOs = Json
      .parse(fundingUTXOsStr)
      .validate[Vector[SerializedSegwitSpendingInfo]]
      .get
      .map(_.toSpendingInfo)

    (privKey, fundingUTXOs)
  }

  def constructAcceptClient(
      offer: DLCOffer,
      accept: DLCAccept,
      acceptPrivKey: ExtPrivateKey,
      acceptFundingUTXOs: Vector[P2WPKHV0SpendingInfo])(
      implicit ec: ExecutionContext): BinaryOutcomeDLCClient = {
    BinaryOutcomeDLCClient.fromOffer(
      offer = offer,
      extPrivKey = acceptPrivKey,
      fundingUtxos = acceptFundingUTXOs,
      totalCollateral = accept.totalCollateral,
      changeSPK =
        accept.changeAddress.scriptPubKey.asInstanceOf[P2WPKHWitnessSPKV0]
    )
  }

  def setupDLCForOffer(
      offerClient: BinaryOutcomeDLCClient,
      accept: DLCAccept): (Transaction => Unit, Future[SetupDLC]) = {
    val fundingTxP = Promise[Transaction]()
    val fillFundingTx = { tx: Transaction =>
      val _ = fundingTxP.success(tx)
    }

    val dlcSetupF = offerClient.setupDLCOffer(
      getSigs = Future.successful(accept.cetSigs),
      sendSigs = (cetSigs, fundingSigs) =>
        Future.successful {
          println()
          println(s"Offer sigs: ${DLCSign(cetSigs, fundingSigs).toJsonStr}")
          println()
        },
      getFundingTx = fundingTxP.future
    )

    (fillFundingTx, dlcSetupF)
  }

  def setupDLCForAccept(
      acceptClient: BinaryOutcomeDLCClient,
      offerSigStr: String): Future[SetupDLC] = {
    val offerSig = DLCSign.fromJson(ujson.read(offerSigStr))

    acceptClient.setupDLCAccept(
      sendSigs = _ => FutureUtil.unit,
      getSigs = Future.successful((offerSig.cetSigs, offerSig.fundingSigs)))
  }

  def demoOffer()(implicit ec: ExecutionContext): Unit = {
    val (oracleInfo, hashA, hashB, sigA, sigB) = Demo.setupOracle()

    val offerStr = ConsoleCli.exec(
      "createdlcoffer",
      "--amount",
      "0.00005",
      "--oracleInfo",
      oracleInfo.hex,
      "--contractInfo",
      s"${hashA.hex},${hashB.hex}",
      "--locktime",
      "1580323752",
      "--refundlocktime",
      "1580323754",
      "--feerate",
      "1"
    )(port = 9999)
    val offer = DLCOffer.fromJson(ujson.read(offerStr.get))

    val offerPrivKeyStr = ???
    val offerFundingUTXOsStr: String = ???
    val (offerPrivKey, offerFundingUTXOs) =
      Demo.parseKeyAndUTXOs(offerPrivKeyStr, offerFundingUTXOsStr)

    // Go give other node your serialized offer string

    val acceptStr = ???
    val accept = DLCAccept.fromJson(ujson.read(acceptStr))

    val offerClient = BinaryOutcomeDLCClient.fromOfferAndAccept(
      offer = offer,
      accept = accept,
      extPrivKey = offerPrivKey,
      fundingUtxos = offerFundingUTXOs)

    val (fundingTxFound, offerSetupF) =
      Demo.setupDLCForOffer(offerClient, accept)

    // Go give other node your serialized cet signatures

    val acceptSetupFundingTx = ???
    fundingTxFound(acceptSetupFundingTx)

    // Pause

    val offerSetup = offerSetupF.value.get.get

    // Setup is now done for both and any execution can happen
  }

  def demoAccept()(implicit ec: ExecutionContext): Unit = {
    val offerStr = ???
    val offer = DLCOffer.fromJson(ujson.read(offerStr))

    val acceptStr = ConsoleCli.exec("acceptdlcoffer",
                                    "--offer",
                                    offerStr,
                                    "--amount",
                                    "0.00005")(port = 9998)
    val accept = DLCAccept.fromJson(ujson.read(acceptStr.get))

    val acceptPrivKeyStr = ???
    val acceptFundingUTXOsStr: String = ???
    val (acceptPrivKey, acceptFundingUTXOs) =
      Demo.parseKeyAndUTXOs(acceptPrivKeyStr, acceptFundingUTXOsStr)

    val acceptClient =
      Demo.constructAcceptClient(offer,
                                 accept,
                                 acceptPrivKey,
                                 acceptFundingUTXOs)

    // Go give other client your serialized accept message

    val offerSigStr = ???
    val acceptSetupF = Demo.setupDLCForAccept(acceptClient, offerSigStr)

    // Pause

    val acceptSetup = acceptSetupF.value.get.get
    println(s"\nFunding Tx: ${acceptSetup.fundingTx.hex}\n")

    // Go give other node the serialized fully-signed funding transaction
  }
}
