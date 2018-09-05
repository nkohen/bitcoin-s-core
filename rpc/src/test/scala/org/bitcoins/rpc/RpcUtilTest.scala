package org.bitcoins.rpc

import java.io.File

import org.scalatest.AsyncFlatSpec

import scala.concurrent.Future
import scala.util.Success

class RpcUtilTest extends AsyncFlatSpec {

  implicit val ec = RpcUtil.system.dispatcher

  private def trueLater(delay: Int = 1000): Future[Boolean] = Future {
    Thread.sleep(delay)
    true
  }

  private def boolLaterDoneAnd(bool: Boolean, boolFuture: Future[Boolean]): Boolean =
    boolFuture.value.contains(Success(bool))

  private def boolLaterDoneAndTrue(trueLater: Future[Boolean]): Boolean =
    boolLaterDoneAnd(true, trueLater)

  behavior of "RpcUtil"

  it should "create a temp bitcoin directory when creating a DaemonInstance, and then delete it" in {
    val instance = RpcUtil.instance(RpcUtil.randomPort, RpcUtil.randomPort)
    val dir = instance.authCredentials.datadir
    assert(dir.isDirectory)
    assert(dir.listFiles.contains(new File(dir.getAbsolutePath + "/bitcoin.conf")))
    RpcUtil.deleteTmpDir(dir)
    assert(!dir.exists)
  }

  it should "complete immediately if condition is true" in {
    RpcUtil.retryUntilSatisfied(condition = true, duration = 0).map { _ =>
      succeed
    }
  }

  it should "fail if condition is false" in {
    recoverToSucceededIf[RuntimeException] {
      RpcUtil.retryUntilSatisfied(condition = false, duration = 0)
    }
  }

  it should "succeed after a delay" in {
    val boolLater = trueLater(delay = 250)
    RpcUtil.retryUntilSatisfied(boolLaterDoneAndTrue(boolLater)).map { _ =>
      succeed
    }
  }

  it should "fail if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    recoverToSucceededIf[RuntimeException] {
      RpcUtil.retryUntilSatisfied(boolLaterDoneAndTrue(boolLater), duration = 0)
    }
  }

  it should "succeed immediately if condition is true" in {
    RpcUtil.awaitCondition(condition = true, 0)
    succeed
  }

  it should "timeout if condition is false" in {
    assertThrows[RuntimeException] {
      RpcUtil.awaitCondition(condition = false, duration = 0)
    }
  }

  it should "block for a delay and then succeed" in {
    val boolLater = trueLater(delay = 250)
    val before: Long = System.currentTimeMillis
    RpcUtil.awaitCondition(boolLaterDoneAndTrue(boolLater))
    val after: Long = System.currentTimeMillis
    assert(after - before >= 250)
  }

  it should "timeout if there is a delay and duration is zero" in {
    val boolLater = trueLater(delay = 250)
    assertThrows[RuntimeException] {
      RpcUtil.awaitCondition(boolLaterDoneAndTrue(boolLater), duration = 0)
    }
  }

  it should "be able to create a connected node pair with 100 blocks and then delete them" in {
    RpcUtil.createNodePair().flatMap {
      case (client1, client2) =>
        assert(client1.getDaemon.authCredentials.datadir.isDirectory)
        assert(client2.getDaemon.authCredentials.datadir.isDirectory)

        client1.getAddedNodeInfo(client2.getDaemon.uri).flatMap { nodes =>
          assert(nodes.nonEmpty)

          client1.getBlockCount.flatMap { count1 =>
            assert(count1 == 100)

            client2.getBlockCount.map { count2 =>
              assert(count2 == 100)

              RpcUtil.deleteNodePair(client1, client2)
              assert(!client1.getDaemon.authCredentials.datadir.exists)
              assert(!client2.getDaemon.authCredentials.datadir.exists)
            }
          }
        }
    }
  }
}
