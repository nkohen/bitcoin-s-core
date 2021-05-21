package org.bitcoins.scripts

import akka.NotUsed
import akka.stream.scaladsl.{Keep, Sink, Source}
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.protocol.transaction.WitnessTransaction
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.server.BitcoindRpcAppConfig
import org.bitcoins.server.routes.BitcoinSRunner

import scala.concurrent.Future

/** Useful script for scanning bitcoind
  * This file assumes you have pre-configured the connection
  * between bitcoin-s and bitcoind inside of bitcoin-s.conf
  * @see https://bitcoin-s.org/docs/config/configuration#example-configuration-file
  */
class ScanBitcoind(override val args: Array[String]) extends BitcoinSRunner {
  override val actorSystemName = "scan-bitcoind"

  implicit val rpcAppConfig: BitcoindRpcAppConfig =
    BitcoindRpcAppConfig(datadir, baseConfig)

  override def start(): Future[Unit] = {

    val bitcoind = rpcAppConfig.client

    val startHeight = 675000
    val endHeightF: Future[Int] = bitcoind.getBlockCount

    for {
      endHeight <- endHeightF
      _ <- countSegwitTxs(bitcoind, startHeight, endHeight)
    } yield {
      sys.exit(0)
    }
  }

  override def stop(): Future[Unit] = {
    system
      .terminate()
      .map(_ => ())
  }

  /** Searches a given Source[Int] that represents block heights applying f to them and returning a Seq[T] with the results */
  def searchBlocks[T](
      bitcoind: BitcoindRpcClient,
      source: Source[Int, NotUsed],
      f: Block => T,
      numParallelism: Int =
        Runtime.getRuntime.availableProcessors() * 2): Future[Seq[T]] = {
    source
      .mapAsync(parallelism = numParallelism) { height =>
        bitcoind
          .getBlockHash(height)
          .flatMap(h => bitcoind.getBlockRaw(h))
          .map(b => (b, height))
      }
      .mapAsync(numParallelism) { case (block, height) =>
        logger.info(
          s"Searching block at height=$height hashBE=${block.blockHeader.hashBE.hex}")
        FutureUtil.makeAsync { () =>
          f(block)
        }
      }
      .toMat(Sink.seq)(Keep.right)
      .run()
  }

  def countSegwitTxs(
      bitcoind: BitcoindRpcClient,
      startHeight: Int,
      endHeight: Int): Future[Unit] = {
    val startTime = System.currentTimeMillis()
    val source: Source[Int, NotUsed] = Source(startHeight.to(endHeight))

    //in this simple example, we are going to count the number of witness transactions
    val countSegwitTxs: Block => Int = { block: Block =>
      block.transactions.count(_.isInstanceOf[WitnessTransaction])
    }
    val countsF: Future[Seq[Int]] = for {
      counts <- searchBlocks[Int](bitcoind, source, countSegwitTxs)
    } yield counts

    val countF: Future[Int] = countsF.map(_.sum)

    for {
      count <- countF
      endTime = System.currentTimeMillis()
      _ = println(
        s"Count of segwit txs from height=${startHeight} to endHeight=${endHeight} is ${count}. It took ${endTime - startTime}ms ")
    } yield ()
  }
}

object ScanBitcoind extends App {
  new ScanBitcoind(args).run()
}
