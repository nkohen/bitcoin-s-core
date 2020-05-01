package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  EmptyScriptWitness,
  ScriptPubKey,
  ScriptWitnessV0
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSigner
import org.bitcoins.core.wallet.utxo.InputInfo
import org.bitcoins.crypto.{DummyECDigitalSignature, Sign}

import scala.concurrent.{ExecutionContext, Future}

sealed trait RawTxFinalizer {

  def buildTx(
      version: Int32,
      inputs: Vector[TransactionInput],
      outputs: Vector[TransactionOutput],
      lockTime: UInt32)(implicit ec: ExecutionContext): Future[Transaction]
}

case object RawFinalizer extends RawTxFinalizer {
  override def buildTx(
      version: Int32,
      inputs: Vector[TransactionInput],
      outputs: Vector[TransactionOutput],
      lockTime: UInt32)(implicit ec: ExecutionContext): Future[Transaction] = {
    Future.successful(BaseTransaction(version, inputs, outputs, lockTime))
  }
}

case object FilterDustFinalizer extends RawTxFinalizer {
  override def buildTx(
      version: Int32,
      inputs: Vector[TransactionInput],
      outputs: Vector[TransactionOutput],
      lockTime: UInt32)(implicit ec: ExecutionContext): Future[Transaction] = {
    val filteredOutputs = outputs.filter(_.value >= Policy.dustThreshold)
    Future.successful(
      BaseTransaction(version, inputs, filteredOutputs, lockTime))
  }
}

case class NonInteractiveWithChangeFinalizer(
    inputInfos: Vector[InputInfo],
    feeRate: FeeUnit,
    changeSPK: ScriptPubKey)
    extends RawTxFinalizer {
  override def buildTx(
      version: Int32,
      inputs: Vector[TransactionInput],
      outputs: Vector[TransactionOutput],
      lockTime: UInt32)(implicit ec: ExecutionContext): Future[Transaction] = {
    val outputsWithChange = outputs :+ TransactionOutput(Satoshis.zero,
                                                         changeSPK)
    val totalCrediting = inputInfos
      .map(_.output.value)
      .foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)
    val totalSpending =
      outputs.map(_.value).foldLeft[CurrencyUnit](Satoshis.zero)(_ + _)
    val witnesses = inputInfos.map(InputInfo.getScriptWitness)
    val txNoChangeFee = TransactionWitness.fromWitOpt(witnesses) match {
      case _: EmptyWitness =>
        BaseTransaction(version, inputs, outputsWithChange, lockTime)
      case wit: TransactionWitness =>
        WitnessTransaction(version, inputs, outputsWithChange, lockTime, wit)
    }

    // Add dummy signatures
    val mockInputFs = inputInfos.zipWithIndex.map {
      case (inputInfo, index) =>
        val mockSigners = inputInfo.pubKeys.map { pubKey =>
          Sign(_ => Future.successful(DummyECDigitalSignature), pubKey)
        }

        val mockSpendingInfo =
          inputInfo.toSpendingInfo(mockSigners, HashType.sigHashAll)

        BitcoinSigner
          .sign(mockSpendingInfo, txNoChangeFee, isDummySignature = true)
          .map(_.transaction)
          .map { tx =>
            val witnessOpt = tx match {
              case _: NonWitnessTransaction => None
              case wtx: WitnessTransaction =>
                wtx.witness.witnesses(index) match {
                  case EmptyScriptWitness   => None
                  case wit: ScriptWitnessV0 => Some(wit)
                }
            }

            (tx.inputs(index), witnessOpt)
          }
    }

    Future.sequence(mockInputFs).map { inputsAndWitnesses =>
      val inputs = inputsAndWitnesses.map(_._1)
      val txWitnesses = inputsAndWitnesses.map(_._2)
      val dummyTx = TransactionWitness.fromWitOpt(txWitnesses) match {
        case _: EmptyWitness =>
          BaseTransaction(version, inputs, outputsWithChange, lockTime)
        case wit: TransactionWitness =>
          WitnessTransaction(version, inputs, outputsWithChange, lockTime, wit)
      }

      val fee = feeRate.calc(dummyTx)
      val change = totalCrediting - totalSpending - fee
      val newChangeOutput = TransactionOutput(change, changeSPK)
      val newOutputs = if (change <= Policy.dustThreshold) {
        outputs
      } else {
        outputs :+ newChangeOutput
      }

      txNoChangeFee match {
        case btx: NonWitnessTransaction =>
          BaseTransaction(btx.version, btx.inputs, newOutputs, btx.lockTime)
        case WitnessTransaction(version, inputs, _, lockTime, witness) =>
          WitnessTransaction(version, inputs, newOutputs, lockTime, witness)
      }
    }
  }
}
