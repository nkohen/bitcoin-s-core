package org.bitcoins.rpc

import akka.actor.ActorSystem
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.RpcClient

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future, Promise}

trait RpcUtil extends BitcoinSLogger {

  private def retryRunnable(condition: => Boolean, p: Promise[Boolean]): Runnable = new Runnable {
    override def run(): Unit = {
      p.success(condition)
    }
  }

  def retryUntilSatisfied(
    condition: => Boolean,
    duration: FiniteDuration = 100.milliseconds,
    counter: Int = 0)(implicit system: ActorSystem): Future[Unit] = {
    implicit val ec = system.dispatcher
    if (counter == 50) {
      Future.failed(new RuntimeException("Condition timed out"))
    } else if (condition) {
      Future.successful(Unit)
    } else {

      val p = Promise[Boolean]()
      val runnable = retryRunnable(condition, p)

      system.scheduler.scheduleOnce(duration, runnable)

      p.future.flatMap {
        case true => Future.successful(Unit)
        case false => retryUntilSatisfied(condition, duration, counter + 1)
      }

    }
  }

  def awaitCondition(
    condition: => Boolean,
    duration: FiniteDuration = 100.milliseconds,
    counter: Int = 0)(implicit system: ActorSystem): Unit = {
    // The timeout is set high so that the user can specify the timeout of the Future freely
    Await.result(retryUntilSatisfied(condition, duration, counter), 1.hour)
  }

  def awaitServer(
    server: RpcClient,
    duration: FiniteDuration = 100.milliseconds,
    counter: Int = 0)(implicit system: ActorSystem): Unit = {
    awaitCondition(server.isStarted, duration, counter)
  }

  def awaitServerShutdown(
    server: RpcClient,
    duration: FiniteDuration = 300.milliseconds,
    counter: Int = 0)(implicit system: ActorSystem): Unit = {
    awaitCondition(!server.isStarted, duration, counter)
  }
}

object RpcUtil extends RpcUtil
