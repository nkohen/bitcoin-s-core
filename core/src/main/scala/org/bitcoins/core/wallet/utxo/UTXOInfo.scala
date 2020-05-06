package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptWitness}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.Sign

sealed trait UTXOInfo[+InputType <: InputInfo] {
  def inputInfo: InputType
  def hashType: HashType
  def signers: Vector[Sign]

  private val keysToSignFor = inputInfo.pubKeys
  require(signers.map(_.publicKey).forall(keysToSignFor.contains),
          s"Cannot have signers that do not sign for one of $keysToSignFor")

  def amount: CurrencyUnit = inputInfo.amount
  def output: TransactionOutput = inputInfo.output
  def outPoint: TransactionOutPoint = inputInfo.outPoint
  def redeemScriptOpt: Option[ScriptPubKey] = inputInfo.redeemScriptOpt
  def scriptWitnessOpt: Option[ScriptWitness] = inputInfo.scriptWitnessOpt
  def conditionalPath: ConditionalPath = inputInfo.conditionalPath
}

object UTXOInfo {
  type Any = UTXOInfo[InputInfo]
  type AnySatisfying = UTXOSatisfyingInfo[InputInfo]
  type AnySigning = UTXOSigningInfo[InputInfo]
}

case class UTXOSatisfyingInfo[+InputType <: InputInfo](
    inputInfo: InputType,
    signers: Vector[Sign],
    hashType: HashType)
    extends UTXOInfo[InputType] {

  def signer: Sign = {
    require(
      signers.length == 1,
      "This method is for spending infos with a single signer, if you mean signers.head be explicit")

    signers.head
  }

  def toSingle(index: Int): UTXOSigningInfo[InputType] = {
    UTXOSigningInfo(inputInfo, signers(index), hashType)
  }

  def toSingles: Vector[UTXOSigningInfo[InputType]] = {
    signers.map { signer =>
      UTXOSigningInfo(inputInfo, signer, hashType)
    }
  }

  def mapInfo[T <: InputInfo](func: InputType => T): UTXOSatisfyingInfo[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }
}

object UTXOSatisfyingInfo {

  def apply[InputType <: InputInfo](
      inputInfo: InputType,
      signer: Sign,
      hashType: HashType): UTXOSatisfyingInfo[InputType] =
    UTXOSatisfyingInfo(inputInfo, Vector(signer), hashType)
}

case class UTXOSigningInfo[+InputType <: InputInfo](
    inputInfo: InputType,
    signer: Sign,
    hashType: HashType)
    extends UTXOInfo[InputType] {
  override def signers: Vector[Sign] = Vector(signer)

  def mapInfo[T <: InputInfo](func: InputType => T): UTXOSigningInfo[T] = {
    this.copy(inputInfo = func(this.inputInfo))
  }
}
