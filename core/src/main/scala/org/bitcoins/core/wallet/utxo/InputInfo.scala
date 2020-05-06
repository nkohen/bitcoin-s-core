package org.bitcoins.core.wallet.utxo

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.script.{
  ConditionalScriptPubKey,
  EmptyScriptPubKey,
  EmptyScriptWitness,
  LockTimeScriptPubKey,
  MultiSignatureScriptPubKey,
  NonStandardScriptPubKey,
  P2PKHScriptPubKey,
  P2PKScriptPubKey,
  P2PKWithTimeoutScriptPubKey,
  P2SHScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0,
  P2WSHWitnessSPKV0,
  P2WSHWitnessV0,
  RawScriptPubKey,
  ScriptPubKey,
  ScriptWitness,
  ScriptWitnessV0,
  UnassignedWitnessScriptPubKey,
  WitnessCommitment,
  WitnessScriptPubKey,
  WitnessScriptPubKeyV0
}
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.{ECPublicKey, Sign}

sealed trait InputInfo {
  def outPoint: TransactionOutPoint

  def amount: CurrencyUnit

  def scriptPubKey: ScriptPubKey

  def output: TransactionOutput = {
    TransactionOutput(amount, scriptPubKey)
  }

  def redeemScriptOpt: Option[ScriptPubKey]

  def scriptWitnessOpt: Option[ScriptWitness]

  def p2pkhPreImage: Option[ECPublicKey]

  def conditionalPath: ConditionalPath

  def pubKeys: Vector[ECPublicKey] = scriptPubKey.pubKeysFor(this)

  def toSpendingInfo(
      signers: Vector[Sign],
      hashType: HashType): UTXOInfo.AnySatisfying = {
    UTXOSatisfyingInfo(this, signers, hashType)
  }

  def toSpendingInfo(signer: Sign, hashType: HashType): UTXOInfo.AnySigning = {
    UTXOSigningInfo(this, signer, hashType)
  }

  def withSignFrom(
      signerMaterial: UTXOInfo.AnySatisfying): UTXOSatisfyingInfo[this.type] = {
    signerMaterial.copy(inputInfo = this)
  }

  def withSignFrom(
      signerMaterial: UTXOInfo.AnySigning): UTXOSigningInfo[this.type] = {
    signerMaterial.copy(inputInfo = this)
  }
}

object InputInfo {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      conditionalPath: ConditionalPath,
      p2pkhPreImageOpt: Option[ECPublicKey] = None): InputInfo = {
    output.scriptPubKey match {
      case _: P2SHScriptPubKey =>
        redeemScriptOpt match {
          case None =>
            throw new IllegalArgumentException(
              "Redeem Script must be defined for P2SH.")
          case Some(redeemScript) =>
            redeemScript match {
              case _: WitnessScriptPubKeyV0 =>
                val witness = scriptWitnessOpt match {
                  case Some(witness: ScriptWitnessV0) => witness
                  case None =>
                    throw new IllegalArgumentException(
                      "Script Witness must be defined for (nested) Segwit input")
                  case Some(_: ScriptWitness) =>
                    throw new UnsupportedOperationException(
                      "Only v0 Segwit is currently supported")
                }
                P2SHNestedSegwitV0InputInfo(outPoint,
                                            output.value,
                                            witness,
                                            conditionalPath,
                                            p2pkhPreImageOpt)
              case nonWitnessSPK: RawScriptPubKey =>
                P2SHNoNestInputInfo(outPoint,
                                    output.value,
                                    nonWitnessSPK,
                                    conditionalPath,
                                    p2pkhPreImageOpt)
              case _: P2SHScriptPubKey =>
                throw new IllegalArgumentException("Cannot have nested P2SH")
              case _: UnassignedWitnessScriptPubKey =>
                throw new UnsupportedOperationException(
                  s"Unsupported ScriptPubKey ${output.scriptPubKey}")
            }
        }
      case _: WitnessScriptPubKeyV0 =>
        val witness = scriptWitnessOpt match {
          case Some(witness: ScriptWitnessV0) => witness
          case None =>
            throw new IllegalArgumentException(
              "Script Witness must be defined for Segwit input")
          case Some(_: ScriptWitness) =>
            throw new UnsupportedOperationException(
              "Only v0 Segwit is currently supported")
        }
        SegwitV0NativeInputInfo(outPoint,
                                output.value,
                                witness,
                                conditionalPath,
                                p2pkhPreImageOpt)
      case wspk: UnassignedWitnessScriptPubKey =>
        UnassignedSegwitNativeInputInfo(
          outPoint,
          output.value,
          wspk,
          scriptWitnessOpt.getOrElse(EmptyScriptWitness),
          conditionalPath)
      case rawSPK: RawScriptPubKey =>
        RawInputInfo(outPoint,
                     output.value,
                     rawSPK,
                     conditionalPath,
                     p2pkhPreImageOpt)
    }
  }
}

sealed trait RawInputInfo extends InputInfo {
  override def scriptPubKey: RawScriptPubKey

  override val redeemScriptOpt: Option[ScriptPubKey] = None

  override val scriptWitnessOpt: Option[ScriptWitness] = None
}

object RawInputInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      conditionalPath: ConditionalPath,
      p2pkhPreImageOpt: Option[ECPublicKey] = None): RawInputInfo = {
    scriptPubKey match {
      case p2pk: P2PKScriptPubKey => P2PKInputInfo(outPoint, amount, p2pk)
      case p2pkh: P2PKHScriptPubKey =>
        p2pkhPreImageOpt match {
          case None =>
            throw new IllegalArgumentException(
              "P2PKH pre-image must be specified for P2PKH ScriptPubKey")
          case Some(p2pkhPreImage) =>
            require(
              P2PKHScriptPubKey(p2pkhPreImage) == p2pkh,
              s"Specified P2PKH pre-image ($p2pkhPreImage) does not match $p2pkh")

            P2PKHInputInfo(outPoint, amount, p2pkhPreImage)
        }
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutInputInfo(outPoint,
                                     amount,
                                     p2pkWithTimeout,
                                     beforeTimeout)
        }
      case multiSig: MultiSignatureScriptPubKey =>
        MultiSignatureInputInfo(outPoint, amount, multiSig)
      case conditional: ConditionalScriptPubKey =>
        ConditionalInputInfo(outPoint,
                             amount,
                             conditional,
                             conditionalPath,
                             p2pkhPreImageOpt)
      case lockTime: LockTimeScriptPubKey =>
        LockTimeInputInfo(outPoint,
                          amount,
                          lockTime,
                          conditionalPath,
                          p2pkhPreImageOpt)
      case EmptyScriptPubKey =>
        EmptyInputInfo(outPoint, amount)
      case _: NonStandardScriptPubKey | _: WitnessCommitment =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey $scriptPubKey")
    }
  }
}

case class EmptyInputInfo(outPoint: TransactionOutPoint, amount: CurrencyUnit)
    extends RawInputInfo {
  override def scriptPubKey: EmptyScriptPubKey.type = EmptyScriptPubKey
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
  override def p2pkhPreImage: Option[ECPublicKey] = None
}

case class P2PKInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKScriptPubKey)
    extends RawInputInfo {
  override def p2pkhPreImage: Option[ECPublicKey] = None

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKHInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    pubKey: ECPublicKey)
    extends RawInputInfo {
  override def scriptPubKey: P2PKHScriptPubKey = P2PKHScriptPubKey(pubKey)

  override def p2pkhPreImage: Option[ECPublicKey] = Some(pubKey)

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKWithTimeoutInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: P2PKWithTimeoutScriptPubKey,
    isBeforeTimeout: Boolean)
    extends RawInputInfo {
  override def p2pkhPreImage: Option[ECPublicKey] = None

  override def conditionalPath: ConditionalPath = {
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
  }
}

case class MultiSignatureInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: MultiSignatureScriptPubKey)
    extends RawInputInfo {
  override def p2pkhPreImage: Option[ECPublicKey] = None

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class ConditionalInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: ConditionalScriptPubKey,
    conditionalPath: ConditionalPath,
    p2pkhPreImage: Option[ECPublicKey] = None)
    extends RawInputInfo {
  lazy val (condition: Boolean, nextConditionalPath: ConditionalPath) =
    conditionalPath match {
      case ConditionalPath.ConditionTrue(nextCondition) =>
        (true, nextCondition)
      case ConditionalPath.ConditionFalse(nextCondition) =>
        (false, nextCondition)
      case ConditionalPath.NoConditionsLeft =>
        throw new IllegalArgumentException("Must specify True or False")
    }

  val nestedInputInfo: RawInputInfo = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawInputInfo(outPoint,
                 amount,
                 nestedSPK,
                 nextConditionalPath,
                 p2pkhPreImage)
  }
}

case class LockTimeInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: LockTimeScriptPubKey,
    conditionalPath: ConditionalPath,
    p2pkhPreImage: Option[ECPublicKey] = None
) extends RawInputInfo {

  val nestedInputInfo: RawInputInfo = RawInputInfo(
    outPoint,
    amount,
    scriptPubKey.nestedScriptPubKey,
    conditionalPath,
    p2pkhPreImage)
}

sealed trait SegwitV0NativeInputInfo extends InputInfo {
  def scriptWitness: ScriptWitnessV0

  override val redeemScriptOpt: Option[ScriptPubKey] = None
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)
}

object SegwitV0NativeInputInfo {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath,
      p2pkhPreImage: Option[ECPublicKey] = None): SegwitV0NativeInputInfo = {
    scriptWitness match {
      case p2wpkh: P2WPKHWitnessV0 =>
        P2WPKHV0InputInfo(outPoint, amount, p2wpkh.pubKey)
      case p2wsh: P2WSHWitnessV0 =>
        P2WSHV0InputInfo(outPoint,
                         amount,
                         p2wsh,
                         conditionalPath,
                         p2pkhPreImage)
    }
  }
}

case class P2WPKHV0InputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    pubKey: ECPublicKey)
    extends SegwitV0NativeInputInfo {
  override def scriptPubKey: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(pubKey)

  override def scriptWitness: P2WPKHWitnessV0 = P2WPKHWitnessV0(pubKey)

  override def p2pkhPreImage: Option[ECPublicKey] = None

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2WSHV0InputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptWitness: P2WSHWitnessV0,
    conditionalPath: ConditionalPath,
    override val p2pkhPreImage: Option[ECPublicKey] = None)
    extends SegwitV0NativeInputInfo {
  override def scriptPubKey: P2WSHWitnessSPKV0 =
    P2WSHWitnessSPKV0(scriptWitness.redeemScript)

  val nestedInputInfo: RawInputInfo =
    RawInputInfo(outPoint,
                 amount,
                 scriptWitness.redeemScript,
                 conditionalPath,
                 p2pkhPreImage)
}

case class UnassignedSegwitNativeInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptPubKey: WitnessScriptPubKey,
    scriptWitness: ScriptWitness,
    conditionalPath: ConditionalPath)
    extends InputInfo {
  override def p2pkhPreImage: Option[ECPublicKey] = None

  override def redeemScriptOpt: Option[ScriptPubKey] = None

  override def scriptWitnessOpt: Option[ScriptWitness] = Some(scriptWitness)
}

sealed trait P2SHInputInfo extends InputInfo {
  override val p2pkhPreImage: Option[ECPublicKey] = None

  def redeemScript: ScriptPubKey

  override val redeemScriptOpt: Option[ScriptPubKey] = Some(redeemScript)

  override def scriptPubKey: P2SHScriptPubKey = P2SHScriptPubKey(redeemScript)

  def nestedInputInfo: InputInfo
}

case class P2SHNoNestInputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    redeemScript: RawScriptPubKey,
    conditionalPath: ConditionalPath,
    override val p2pkhPreImage: Option[ECPublicKey] = None)
    extends P2SHInputInfo {
  override val scriptWitnessOpt: Option[ScriptWitnessV0] = None

  override val nestedInputInfo: RawInputInfo =
    RawInputInfo(outPoint, amount, redeemScript, conditionalPath, p2pkhPreImage)
}

case class P2SHNestedSegwitV0InputInfo(
    outPoint: TransactionOutPoint,
    amount: CurrencyUnit,
    scriptWitness: ScriptWitnessV0,
    conditionalPath: ConditionalPath,
    override val p2pkhPreImage: Option[ECPublicKey] = None)
    extends P2SHInputInfo {
  override def scriptWitnessOpt: Option[ScriptWitnessV0] = Some(scriptWitness)

  override def redeemScript: WitnessScriptPubKeyV0 = scriptWitness match {
    case p2wpkh: P2WPKHWitnessV0 => P2WPKHWitnessSPKV0(p2wpkh.pubKey)
    case p2wsh: P2WSHWitnessV0   => P2WSHWitnessSPKV0(p2wsh.redeemScript)
  }

  override val nestedInputInfo: SegwitV0NativeInputInfo =
    SegwitV0NativeInputInfo(outPoint,
                            amount,
                            scriptWitness,
                            conditionalPath,
                            p2pkhPreImage)
}