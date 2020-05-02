package org.bitcoins.core.wallet.builder

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutput
}

import scala.collection.mutable

case class RawTxBuilder() extends mutable.Clearable {
  private var version: Int32 = TransactionConstants.validLockVersion

  private val inputsBuilder: mutable.ReusableBuilder[
    TransactionInput,
    Vector[TransactionInput]] = Vector.newBuilder

  private val outputsBuilder: mutable.ReusableBuilder[
    TransactionOutput,
    Vector[TransactionOutput]] = Vector.newBuilder

  private var lockTime: UInt32 = TransactionConstants.lockTime

  def result(): BaseTransaction = {
    BaseTransaction(version,
                    inputsBuilder.result(),
                    outputsBuilder.result(),
                    lockTime)
  }

  override def clear(): Unit = {
    version = TransactionConstants.validLockVersion
    inputsBuilder.clear()
    outputsBuilder.clear()
    lockTime = TransactionConstants.lockTime
  }

  def addInput(input: TransactionInput): this.type = {
    inputsBuilder += input
    this
  }

  @inline final def +=(input: TransactionInput): this.type = addInput(input)

  def addInputs(inputs: IterableOnce[TransactionInput]): this.type = {
    inputsBuilder ++= inputs
    this
  }

  def addOutput(output: TransactionOutput): this.type = {
    outputsBuilder += output
    this
  }

  @inline final def +=(output: TransactionOutput): this.type = addOutput(output)

  def addOutputs(outputs: IterableOnce[TransactionOutput]): this.type = {
    outputsBuilder ++= outputs
    this
  }

  @inline final def ++=[T >: TransactionInput with TransactionOutput](
      inputsOrOutputs: IterableOnce[T]): this.type = {
    val vec = inputsOrOutputs.iterator.toVector
    val inputs = vec.collect {
      case input: TransactionInput => input
    }
    val outputs = vec.collect {
      case output: TransactionOutput => output
    }

    addInputs(inputs)
    addOutputs(outputs)
  }

  def setVersion(version: Int32): this.type = {
    this.version = version
    this
  }

  def setLockTime(lockTime: UInt32): this.type = {
    this.lockTime = lockTime
    this
  }
}

object RawTxBuilder {
  private def build(
      version: Option[Int32],
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      lockTime: Option[UInt32]): RawTxBuilder = {
    val builder = RawTxBuilder()

    version match {
      case Some(version) => builder.setVersion(version)
      case None          => ()
    }

    if (inputs.nonEmpty) {
      builder ++= inputs
    }

    if (outputs.nonEmpty) {
      builder ++= outputs
    }

    lockTime match {
      case Some(lockTime) => builder.setLockTime(lockTime)
      case None           => ()
    }

    builder
  }

  def apply(version: Int32): RawTxBuilder =
    build(Some(version), Vector.empty, Vector.empty, None)

  def apply[T >: TransactionInput with TransactionOutput](
      inputsOrOutputs: Seq[T]): RawTxBuilder = {
    val vec = inputsOrOutputs.iterator.toVector
    val inputs = vec.collect {
      case input: TransactionInput => input
    }
    val outputs = vec.collect {
      case output: TransactionOutput => output
    }

    build(None, inputs, outputs, None)
  }

  def apply(lockTime: UInt32): RawTxBuilder =
    build(None, Vector.empty, Vector.empty, Some(lockTime))

  def apply[T >: TransactionInput with TransactionOutput](
      version: Int32,
      inputsOrOutputs: Seq[T]): RawTxBuilder = {
    RawTxBuilder(inputsOrOutputs).setVersion(version)
  }

  def apply[T >: TransactionInput with TransactionOutput](
      inputsOrOutputs: Seq[T],
      lockTime: UInt32): RawTxBuilder = {
    RawTxBuilder(inputsOrOutputs)
      .setLockTime(lockTime)
  }

  def apply[T >: TransactionInput with TransactionOutput](
      version: Int32,
      inputsOrOutputs: Seq[T],
      lockTime: UInt32): RawTxBuilder = {
    RawTxBuilder(inputsOrOutputs)
      .setVersion(version)
      .setLockTime(lockTime)
  }

  def apply(
      version: Int32,
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput]): RawTxBuilder =
    build(Some(version), inputs, outputs, None)

  def apply(
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput]): RawTxBuilder =
    build(None, inputs, outputs, None)

  def apply(
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      lockTime: UInt32): RawTxBuilder =
    build(None, inputs, outputs, Some(lockTime))

  def apply(
      version: Int32,
      inputs: Seq[TransactionInput],
      outputs: Seq[TransactionOutput],
      lockTime: UInt32): RawTxBuilder =
    build(Some(version), inputs, outputs, Some(lockTime))

  def apply(version: Int32, lockTime: UInt32): RawTxBuilder =
    build(Some(version), Vector.empty, Vector.empty, Some(lockTime))
}
