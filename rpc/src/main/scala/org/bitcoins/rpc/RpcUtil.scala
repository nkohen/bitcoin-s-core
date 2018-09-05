package org.bitcoins.rpc

import akka.actor.ActorSystem
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.rpc.client.RpcClient

import scala.concurrent.duration.DurationInt
import scala.concurrent.{ Await, Future, Promise }

trait RpcUtil extends BitcoinSLogger {
  def retryUntilSatisfied(condition: => Boolean, duration: Int = 100, counter: Int = 0)(implicit system: ActorSystem): Future[Unit] = {
    implicit val ec = system.dispatcher
    def retryRunnable(promise: Promise[Future[Unit]]): Runnable = new Runnable {
      override def run(): Unit = {
        promise.success(retryUntilSatisfied(condition, duration, counter + 1))
      }
    }

    if (counter == 50) {
      Future.failed(new RuntimeException("Condition timed out"))
    } else if (condition) {
      Future.successful(Unit)
    } else {
      val promise = Promise[Future[Unit]]()
      system.scheduler.scheduleOnce(duration.milliseconds, retryRunnable(promise))
      promise.future.flatMap(f => f)
    }
  }

  def awaitCondition(
    condition: => Boolean,
    duration: Int = 100,
    counter: Int = 0)(implicit system: ActorSystem): Unit = {
    // The timeout is set high so that the user can specify the timeout of the Future freely
    Await.result(retryUntilSatisfied(condition, duration, counter), 1.hour)
  }

  def awaitServer(
    server: RpcClient,
    duration: Int = 100,
    counter: Int = 0)(implicit system: ActorSystem): Unit = {
    awaitCondition(server.isStarted, duration, counter)
  }

  def awaitServerShutdown(
    server: RpcClient,
    duration: Int = 300,
    counter: Int = 0)(implicit system: ActorSystem): Unit = {
    awaitCondition(!server.isStarted, duration, counter)
  }
}

object RpcUtil extends RpcUtil
