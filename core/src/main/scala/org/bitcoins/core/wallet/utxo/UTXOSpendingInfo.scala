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
import org.bitcoins.crypto.{CryptoUtil, Sign}

sealed abstract class UTXOSpendingInfo {

  def inputInfo: InputInfo

  /** The funding transaction's txid and the index of the output in the transaction we are spending */
  def outPoint: TransactionOutPoint = inputInfo.outPoint

  def amount: CurrencyUnit = inputInfo.amount

  def scriptPubKey: ScriptPubKey = inputInfo.scriptPubKey

  /** the actual output itself we are spending */
  def output: TransactionOutput = inputInfo.output

  /** The signer signing in the output above */
  def signers: Vector[Sign]

  def hashType: HashType

  def redeemScriptOpt: Option[ScriptPubKey] = inputInfo.redeemScriptOpt

  def scriptWitnessOpt: Option[ScriptWitness] = inputInfo.scriptWitnessOpt

  def conditionalPath: ConditionalPath = inputInfo.conditionalPath
}

/**
  * Contains the information required to sign an unspent transaction output (UTXO) for a single key
  */
sealed trait UTXOSpendingInfoSingle extends UTXOSpendingInfo {

  /** The signer signing in the output above */
  def signer: Sign

  override def signers: Vector[Sign] = Vector(signer)
}

/**
  * Contains the information required to fully sign an unspent transaction output (UTXO)
  * on a blockchain.
  *
  * If you want to partially sign a UTXO you will likely need to use UTXOSpendingInfoSingle.
  */
sealed trait UTXOSpendingInfoFull extends UTXOSpendingInfo {
  def requiredSigs: Int

  def toSingle(signerIndex: Int): UTXOSpendingInfoSingle

  /** Generates a UTXOSpendingInfoSingle for every Sign required to spend this
    * UTXO. Note that if more keys than necessary are specified, only the first
    * requiredSigs specified will be taken here
    */
  def toSingles: Vector[UTXOSpendingInfoSingle] = {
    signers.indices.take(requiredSigs).toVector.map(toSingle)
  }
}

sealed trait BitcoinUTXOSpendingInfo extends UTXOSpendingInfo {
  protected def isValidScriptWitness(
      spk: WitnessScriptPubKeyV0,
      scriptWitness: ScriptWitnessV0): Boolean = {
    BitcoinUTXOSpendingInfo.isValidScriptWitness(spk, scriptWitness)
  }
}

object BitcoinUTXOSpendingInfo {

  def isValidScriptWitness(
      spk: WitnessScriptPubKeyV0,
      scriptWitness: ScriptWitnessV0): Boolean = {
    spk match {
      case p2wpkh: P2WPKHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WPKHWitnessV0 =>
            CryptoUtil.sha256Hash160(witness.pubKey.bytes) == p2wpkh.pubKeyHash
          case _: ScriptWitnessV0 => false
        }
      case p2wsh: P2WSHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WSHWitnessV0 =>
            CryptoUtil.sha256(witness.redeemScript.asmBytes) == p2wsh.scriptHash
          case _: ScriptWitnessV0 => false
        }
    }
  }
}

sealed trait BitcoinUTXOSpendingInfoSingle
    extends UTXOSpendingInfoSingle
    with BitcoinUTXOSpendingInfo

object BitcoinUTXOSpendingInfoSingle {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      signer: Sign,
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      hashType: HashType,
      conditionalPath: ConditionalPath): BitcoinUTXOSpendingInfoSingle = {
    output.scriptPubKey match {
      case p2sh: P2SHScriptPubKey =>
        redeemScriptOpt match {
          case None =>
            throw new IllegalArgumentException(
              "Redeem Script must be defined for P2SH.")
          case Some(redeemScript) =>
            require(P2SHScriptPubKey(redeemScript) == p2sh,
                    s"Redeem script $redeemScript not valid for $p2sh")

            redeemScript match {
              case _: WitnessScriptPubKeyV0 =>
                val witnessOpt = scriptWitnessOpt match {
                  case Some(witness: ScriptWitnessV0) => Some(witness)
                  case None                           => None
                  case Some(_: ScriptWitness) =>
                    throw new UnsupportedOperationException(
                      "Only v0 Segwit is currently supported")
                }
                P2SHNestedSegwitV0UTXOSpendingInfoSingle(
                  P2SHNestedSegwitV0InputInfo(
                    outPoint,
                    output.value,
                    witnessOpt.getOrElse(throw new IllegalArgumentException(
                      "Script Witness must be defined for (nested) Segwit input")),
                    conditionalPath,
                    Some(signer.publicKey)
                  ),
                  signer,
                  hashType
                )
              case nonWitnessSPK: RawScriptPubKey =>
                P2SHNoNestSpendingInfoSingle(
                  P2SHNoNestInputInfo(outPoint,
                                      output.value,
                                      nonWitnessSPK,
                                      conditionalPath,
                                      Some(signer.publicKey)),
                  signer,
                  hashType)
              case _: P2SHScriptPubKey =>
                throw new IllegalArgumentException("Cannot have nested P2SH")
              case _: UnassignedWitnessScriptPubKey =>
                throw new UnsupportedOperationException(
                  s"Unsupported ScriptPubKey ${output.scriptPubKey}")
            }
        }
      case wspk: WitnessScriptPubKeyV0 =>
        val witnessOpt = scriptWitnessOpt match {
          case Some(witness: ScriptWitnessV0) => Some(witness)
          case None                           => None
          case Some(_: ScriptWitness) =>
            throw new UnsupportedOperationException(
              "Only v0 Segwit is currently supported")
        }

        SegwitV0NativeUTXOSpendingInfoSingle(
          outPoint,
          output.value,
          wspk,
          signer,
          hashType,
          witnessOpt.getOrElse(
            throw new IllegalArgumentException(
              "Script Witness must be defined for Segwit input")),
          conditionalPath
        )
      case wspk: UnassignedWitnessScriptPubKey =>
        UnassignedSegwitNativeUTXOSpendingInfo(
          UnassignedSegwitNativeInputInfo(
            outPoint,
            output.value,
            wspk,
            scriptWitnessOpt.getOrElse(EmptyScriptWitness),
            conditionalPath),
          Vector(signer),
          hashType)
      case rawSPK: RawScriptPubKey =>
        RawScriptUTXOSpendingInfoSingle(outPoint,
                                        output.value,
                                        rawSPK,
                                        signer,
                                        hashType,
                                        conditionalPath)
    }
  }
}

sealed trait BitcoinUTXOSpendingInfoFull
    extends UTXOSpendingInfoFull
    with BitcoinUTXOSpendingInfo {

  override def toSingle(signerIndex: Int): BitcoinUTXOSpendingInfoSingle = {
    BitcoinUTXOSpendingInfoSingle(outPoint,
                                  output,
                                  signers(signerIndex),
                                  redeemScriptOpt,
                                  scriptWitnessOpt,
                                  hashType,
                                  conditionalPath)
  }
}

object BitcoinUTXOSpendingInfoFull {

  def apply(
      outPoint: TransactionOutPoint,
      output: TransactionOutput,
      signers: Vector[Sign],
      redeemScriptOpt: Option[ScriptPubKey],
      scriptWitnessOpt: Option[ScriptWitness],
      hashType: HashType,
      conditionalPath: ConditionalPath): BitcoinUTXOSpendingInfoFull = {
    val firstPubKeyOpt = signers.headOption.map(_.publicKey)

    output.scriptPubKey match {
      case p2sh: P2SHScriptPubKey =>
        redeemScriptOpt match {
          case None =>
            throw new IllegalArgumentException(
              "Redeem Script must be defined for P2SH.")
          case Some(redeemScript) =>
            require(P2SHScriptPubKey(redeemScript) == p2sh,
                    s"Redeem script $redeemScript not valid for $p2sh")

            redeemScript match {
              case _: WitnessScriptPubKeyV0 =>
                val witnessOpt = scriptWitnessOpt match {
                  case Some(witness: ScriptWitnessV0) => Some(witness)
                  case None                           => None
                  case Some(_: ScriptWitness) =>
                    throw new UnsupportedOperationException(
                      "Only v0 Segwit is currently supported")
                }
                P2SHNestedSegwitV0UTXOSpendingInfoFull(
                  P2SHNestedSegwitV0InputInfo(
                    outPoint,
                    output.value,
                    witnessOpt.getOrElse(throw new IllegalArgumentException(
                      "Script Witness must be defined for (nested) Segwit input")),
                    conditionalPath,
                    firstPubKeyOpt
                  ),
                  signers,
                  hashType
                )
              case nonWitnessSPK: RawScriptPubKey =>
                P2SHNoNestSpendingInfoFull(P2SHNoNestInputInfo(outPoint,
                                                               output.value,
                                                               nonWitnessSPK,
                                                               conditionalPath,
                                                               firstPubKeyOpt),
                                           signers,
                                           hashType)
              case _: P2SHScriptPubKey =>
                throw new IllegalArgumentException("Cannot have nested P2SH")
              case _: UnassignedWitnessScriptPubKey =>
                throw new UnsupportedOperationException(
                  s"Unsupported ScriptPubKey ${output.scriptPubKey}")
            }
        }
      case wspk: WitnessScriptPubKeyV0 =>
        val witnessOpt = scriptWitnessOpt match {
          case Some(witness: ScriptWitnessV0) => Some(witness)
          case None                           => None
          case Some(_: ScriptWitness) =>
            throw new UnsupportedOperationException(
              "Only v0 Segwit is currently supported")
        }

        SegwitV0NativeUTXOSpendingInfoFull(
          outPoint,
          output.value,
          wspk,
          signers,
          hashType,
          witnessOpt.getOrElse(
            throw new IllegalArgumentException(
              "Script Witness must be defined for Segwit input")),
          conditionalPath
        )
      case wspk: UnassignedWitnessScriptPubKey =>
        UnassignedSegwitNativeUTXOSpendingInfo(
          UnassignedSegwitNativeInputInfo(
            outPoint,
            output.value,
            wspk,
            scriptWitnessOpt.getOrElse(EmptyScriptWitness),
            conditionalPath),
          signers,
          hashType)
      case rawSPK: RawScriptPubKey =>
        RawScriptUTXOSpendingInfoFull(outPoint,
                                      output.value,
                                      rawSPK,
                                      signers,
                                      hashType,
                                      conditionalPath)
    }
  }

  def apply(
      spendingInfoSingle: UTXOSpendingInfoSingle,
      signers: Vector[Sign]): BitcoinUTXOSpendingInfoFull = {
    require(signers.contains(spendingInfoSingle.signer),
            s"Signer from spendingInfoSingle missing in signers, got: $signers")
    BitcoinUTXOSpendingInfoFull(
      outPoint = spendingInfoSingle.outPoint,
      output = spendingInfoSingle.output,
      signers = signers,
      redeemScriptOpt = spendingInfoSingle.redeemScriptOpt,
      scriptWitnessOpt = spendingInfoSingle.scriptWitnessOpt,
      hashType = spendingInfoSingle.hashType,
      conditionalPath = spendingInfoSingle.conditionalPath
    )
  }

  def unapply(info: BitcoinUTXOSpendingInfoFull): Option[
    (
        TransactionOutPoint,
        TransactionOutput,
        Seq[Sign],
        Option[ScriptPubKey],
        Option[ScriptWitness],
        HashType,
        ConditionalPath)] = {
    Some(info.outPoint,
         info.output,
         info.signers,
         info.redeemScriptOpt,
         info.scriptWitnessOpt,
         info.hashType,
         info.conditionalPath)
  }
}

sealed trait RawScriptUTXOSpendingInfo extends BitcoinUTXOSpendingInfo {
  override def inputInfo: RawInputInfo

  override def scriptPubKey: RawScriptPubKey = inputInfo.scriptPubKey
}

/** This represents the information needed to be sign, with a single key, scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or
  * [[org.bitcoins.core.protocol.script.MultiSignatureScriptPubKey multisig]] scripts.
  * Basically this is for ScriptPubKeys where there is no nesting that requires a redeem script
  */
sealed trait RawScriptUTXOSpendingInfoSingle
    extends BitcoinUTXOSpendingInfoSingle
    with RawScriptUTXOSpendingInfo

object RawScriptUTXOSpendingInfoSingle {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signer: Sign,
      hashType: HashType,
      conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfoSingle = {
    scriptPubKey match {
      case p2pk: P2PKScriptPubKey =>
        P2PKSpendingInfo(P2PKInputInfo(outPoint, amount, p2pk),
                         signer,
                         hashType)
      case p2pkh: P2PKHScriptPubKey =>
        require(p2pkh == P2PKHScriptPubKey(signer.publicKey),
                "Signer pubkey must match ScriptPubKey")

        P2PKHSpendingInfo(P2PKHInputInfo(outPoint, amount, signer.publicKey),
                          signer,
                          hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutSpendingInfo(
              P2PKWithTimeoutInputInfo(outPoint,
                                       amount,
                                       p2pkWithTimeout,
                                       beforeTimeout),
              signer,
              hashType)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureSpendingInfoSingle(
          MultiSignatureInputInfo(outPoint, amount, multisig),
          signer,
          hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeSpendingInfoSingle(LockTimeInputInfo(outPoint,
                                                     amount,
                                                     locktime,
                                                     conditionalPath,
                                                     Some(signer.publicKey)),
                                   signer,
                                   hashType)
      case conditional: ConditionalScriptPubKey =>
        ConditionalSpendingInfoSingle(
          ConditionalInputInfo(outPoint,
                               amount,
                               conditional,
                               conditionalPath,
                               Some(signer.publicKey)),
          signer,
          hashType)
      case EmptyScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey for single signing: $scriptPubKey")
    }
  }
}

/** This represents the information needed to be spend scripts like
  * [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey p2pkh]] or [[org.bitcoins.core.protocol.script.P2PKScriptPubKey p2pk]]
  * scripts. Basically there is no nesting that requires a redeem script here*/
sealed trait RawScriptUTXOSpendingInfoFull
    extends BitcoinUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfo

object RawScriptUTXOSpendingInfoFull {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: RawScriptPubKey,
      signers: Seq[Sign],
      hashType: HashType,
      conditionalPath: ConditionalPath): RawScriptUTXOSpendingInfoFull = {
    val firstPubKeyOpt = signers.headOption.map(_.publicKey)

    scriptPubKey match {
      case p2pk: P2PKScriptPubKey =>
        P2PKSpendingInfo(P2PKInputInfo(outPoint, amount, p2pk),
                         signers.head,
                         hashType)
      case p2pkh: P2PKHScriptPubKey =>
        require(p2pkh == P2PKHScriptPubKey(signers.head.publicKey),
                "Signer pubkey must match ScriptPubKey")

        P2PKHSpendingInfo(
          P2PKHInputInfo(outPoint, amount, signers.head.publicKey),
          signers.head,
          hashType)
      case p2pkWithTimeout: P2PKWithTimeoutScriptPubKey =>
        conditionalPath.headOption match {
          case None =>
            throw new IllegalArgumentException(
              "ConditionalPath must be specified for P2PKWithTimeout")
          case Some(beforeTimeout) =>
            P2PKWithTimeoutSpendingInfo(
              P2PKWithTimeoutInputInfo(outPoint,
                                       amount,
                                       p2pkWithTimeout,
                                       beforeTimeout),
              signers.head,
              hashType)
        }
      case multisig: MultiSignatureScriptPubKey =>
        MultiSignatureSpendingInfoFull(
          MultiSignatureInputInfo(outPoint, amount, multisig),
          signers.toVector,
          hashType)
      case locktime: LockTimeScriptPubKey =>
        LockTimeSpendingInfoFull(LockTimeInputInfo(outPoint,
                                                   amount,
                                                   locktime,
                                                   conditionalPath,
                                                   firstPubKeyOpt),
                                 signers.toVector,
                                 hashType)
      case conditional: ConditionalScriptPubKey =>
        ConditionalSpendingInfoFull(ConditionalInputInfo(outPoint,
                                                         amount,
                                                         conditional,
                                                         conditionalPath,
                                                         firstPubKeyOpt),
                                    signers.toVector,
                                    hashType)
      case EmptyScriptPubKey =>
        EmptySpendingInfo(EmptyInputInfo(outPoint, amount), hashType)
      case _: NonStandardScriptPubKey | _: WitnessCommitment =>
        throw new UnsupportedOperationException(
          s"Currently unsupported ScriptPubKey $scriptPubKey")
    }
  }
}

/** For spending EmptyScriptPubKeys in tests. Probably should not be used in real life */
case class EmptySpendingInfo(inputInfo: EmptyInputInfo, hashType: HashType)
    extends RawScriptUTXOSpendingInfoFull {
  override def signers: Vector[Sign] = Vector.empty
  override def requiredSigs: Int = 0
}

case class P2PKSpendingInfo(
    inputInfo: P2PKInputInfo,
    override val signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfoSingle {
  require(scriptPubKey.publicKey == signer.publicKey,
          "Signer pubkey must match ScriptPubKey")

  override def scriptPubKey: P2PKScriptPubKey = inputInfo.scriptPubKey

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKHSpendingInfo(
    inputInfo: P2PKHInputInfo,
    override val signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfoSingle {
  override def scriptPubKey: P2PKHScriptPubKey = inputInfo.scriptPubKey

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

case class P2PKWithTimeoutSpendingInfo(
    inputInfo: P2PKWithTimeoutInputInfo,
    override val signer: Sign,
    hashType: HashType)
    extends RawScriptUTXOSpendingInfoFull
    with RawScriptUTXOSpendingInfoSingle {
  require(
    scriptPubKey.pubKey == signer.publicKey || scriptPubKey.timeoutPubKey == signer.publicKey,
    "Signer pubkey must match ScriptPubKey")

  override def scriptPubKey: P2PKWithTimeoutScriptPubKey =
    inputInfo.scriptPubKey

  def isBeforeTimeout: Boolean = inputInfo.isBeforeTimeout

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    if (isBeforeTimeout) {
      ConditionalPath.nonNestedTrue
    } else {
      ConditionalPath.nonNestedFalse
    }
}

sealed trait MultiSignatureSpendingInfo extends RawScriptUTXOSpendingInfo {
  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
  override def inputInfo: MultiSignatureInputInfo
  override def scriptPubKey: MultiSignatureScriptPubKey = inputInfo.scriptPubKey
}

case class MultiSignatureSpendingInfoSingle(
    inputInfo: MultiSignatureInputInfo,
    signer: Sign,
    hashType: HashType
) extends MultiSignatureSpendingInfo
    with RawScriptUTXOSpendingInfoSingle

case class MultiSignatureSpendingInfoFull(
    inputInfo: MultiSignatureInputInfo,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType
) extends RawScriptUTXOSpendingInfoFull
    with MultiSignatureSpendingInfo {
  require(signersWithPossibleExtra.length >= scriptPubKey.requiredSigs,
          s"Not enough signers!: $this")

  override val requiredSigs: Int = scriptPubKey.requiredSigs

  override val signers: Vector[Sign] =
    signersWithPossibleExtra.take(requiredSigs)

  override def toSingle(signerIndex: Int): MultiSignatureSpendingInfoSingle = {
    MultiSignatureSpendingInfoSingle(inputInfo, signers(signerIndex), hashType)
  }

  /** @inheritdoc */
  override def toSingles: Vector[MultiSignatureSpendingInfoSingle] = {
    signers.map { signer =>
      MultiSignatureSpendingInfoSingle(inputInfo, signer, hashType)
    }
  }
}

sealed trait ConditionalSpendingInfo extends RawScriptUTXOSpendingInfo {
  require(conditionalPath != ConditionalPath.NoConditionsLeft,
          "Must specify True or False")

  override def inputInfo: ConditionalInputInfo

  override def scriptPubKey: ConditionalScriptPubKey = inputInfo.scriptPubKey

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo

  lazy val (condition: Boolean, nextConditionalPath: ConditionalPath) =
    conditionalPath match {
      case ConditionalPath.ConditionTrue(nextCondition) =>
        (true, nextCondition)
      case ConditionalPath.ConditionFalse(nextCondition) =>
        (false, nextCondition)
      case ConditionalPath.NoConditionsLeft =>
        throw new IllegalStateException(
          "This should be covered by invariant above")
    }
}

case class ConditionalSpendingInfoSingle(
    inputInfo: ConditionalInputInfo,
    signer: Sign,
    hashType: HashType)
    extends ConditionalSpendingInfo
    with RawScriptUTXOSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    nestedSPK,
                                    signer,
                                    hashType,
                                    nextConditionalPath)
  }
}

/** Info required for signing a [[ConditionalScriptPubKey]] */
case class ConditionalSpendingInfoFull(
    inputInfo: ConditionalInputInfo,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType)
    extends RawScriptUTXOSpendingInfoFull
    with ConditionalSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull = {
    val nestedSPK = if (condition) {
      scriptPubKey.trueSPK
    } else {
      scriptPubKey.falseSPK
    }

    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  nestedSPK,
                                  signersWithPossibleExtra,
                                  hashType,
                                  nextConditionalPath)
  }

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

sealed trait LockTimeSpendingInfo extends RawScriptUTXOSpendingInfo {
  override def inputInfo: LockTimeInputInfo

  override def scriptPubKey: LockTimeScriptPubKey = inputInfo.scriptPubKey

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo
}

case class LockTimeSpendingInfoSingle(
    inputInfo: LockTimeInputInfo,
    signer: Sign,
    hashType: HashType
) extends LockTimeSpendingInfo
    with RawScriptUTXOSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    scriptPubKey.nestedScriptPubKey,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }
}

case class LockTimeSpendingInfoFull(
    inputInfo: LockTimeInputInfo,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType
) extends RawScriptUTXOSpendingInfoFull
    with LockTimeSpendingInfo {

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull = {
    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  scriptPubKey.nestedScriptPubKey,
                                  signersWithPossibleExtra,
                                  hashType,
                                  conditionalPath)
  }

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

sealed trait SegwitV0NativeUTXOSpendingInfo extends BitcoinUTXOSpendingInfo {
  override def inputInfo: SegwitV0NativeInputInfo

  def scriptWitness: ScriptWitnessV0 = inputInfo.scriptWitness
}

/** This is the case where we are signing a
  * [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]
  */
sealed trait SegwitV0NativeUTXOSpendingInfoSingle
    extends SegwitV0NativeUTXOSpendingInfo
    with BitcoinUTXOSpendingInfoSingle

object SegwitV0NativeUTXOSpendingInfoSingle {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signer: Sign,
      hashType: HashType,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath): SegwitV0NativeUTXOSpendingInfoSingle = {
    scriptPubKey match {
      case p2wsh: P2WSHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WSHWitnessV0 =>
            require(P2WSHWitnessSPKV0(witness.redeemScript) == p2wsh,
                    "Witness has incorrect redeem script")

            P2WSHV0SpendingInfoSingle(P2WSHV0InputInfo(outPoint,
                                                       amount,
                                                       witness,
                                                       conditionalPath,
                                                       Some(signer.publicKey)),
                                      signer,
                                      hashType)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WPKH")
        }
      case p2wpkh: P2WPKHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WPKHWitnessV0 =>
            require(P2WPKHWitnessSPKV0(witness.pubKey) == p2wpkh,
                    "Witness has incorrect public key")

            P2WPKHV0SpendingInfo(
              P2WPKHV0InputInfo(outPoint, amount, witness.pubKey),
              signer,
              hashType)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WPKH")
        }
    }
  }
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
sealed trait SegwitV0NativeUTXOSpendingInfoFull
    extends BitcoinUTXOSpendingInfoFull
    with SegwitV0NativeUTXOSpendingInfo

object SegwitV0NativeUTXOSpendingInfoFull {

  def apply(
      outPoint: TransactionOutPoint,
      amount: CurrencyUnit,
      scriptPubKey: WitnessScriptPubKeyV0,
      signers: Seq[Sign],
      hashType: HashType,
      scriptWitness: ScriptWitnessV0,
      conditionalPath: ConditionalPath): SegwitV0NativeUTXOSpendingInfoFull = {
    scriptPubKey match {
      case p2wpkh: P2WPKHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WPKHWitnessV0 =>
            require(P2WPKHWitnessSPKV0(witness.pubKey) == p2wpkh,
                    "Witness has incorrect public key")

            P2WPKHV0SpendingInfo(
              P2WPKHV0InputInfo(outPoint, amount, witness.pubKey),
              signers.head,
              hashType)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WPKH")
        }
      case p2wsh: P2WSHWitnessSPKV0 =>
        scriptWitness match {
          case witness: P2WSHWitnessV0 =>
            require(P2WSHWitnessSPKV0(witness.redeemScript) == p2wsh,
                    "Witness has incorrect redeem script")

            val pubKeyOpt = signers.headOption.map(_.publicKey)

            P2WSHV0SpendingInfoFull(P2WSHV0InputInfo(outPoint,
                                                     amount,
                                                     witness,
                                                     conditionalPath,
                                                     pubKeyOpt),
                                    signers.toVector,
                                    hashType)
          case _: ScriptWitnessV0 =>
            throw new IllegalArgumentException("Script witness must be P2WSH")
        }
    }
  }
}

case class P2WPKHV0SpendingInfo(
    inputInfo: P2WPKHV0InputInfo,
    override val signer: Sign,
    hashType: HashType)
    extends SegwitV0NativeUTXOSpendingInfoFull
    with SegwitV0NativeUTXOSpendingInfoSingle {
  require(scriptWitness.pubKey == signer.publicKey,
          "Witness has incorrect public key")

  override def scriptPubKey: P2WPKHWitnessSPKV0 = inputInfo.scriptPubKey

  override def scriptWitness: P2WPKHWitnessV0 = inputInfo.scriptWitness

  override val requiredSigs: Int = 1

  override def conditionalPath: ConditionalPath =
    ConditionalPath.NoConditionsLeft
}

sealed trait P2WSHV0SpendingInfo extends SegwitV0NativeUTXOSpendingInfo {
  override def inputInfo: P2WSHV0InputInfo

  override def scriptPubKey: P2WSHWitnessSPKV0 = inputInfo.scriptPubKey
  override def scriptWitness: P2WSHWitnessV0 = inputInfo.scriptWitness

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo
}

case class P2WSHV0SpendingInfoSingle(
    inputInfo: P2WSHV0InputInfo,
    signer: Sign,
    hashType: HashType)
    extends P2WSHV0SpendingInfo
    with SegwitV0NativeUTXOSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    scriptWitness.redeemScript,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }
}

case class P2WSHV0SpendingInfoFull(
    inputInfo: P2WSHV0InputInfo,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType)
    extends SegwitV0NativeUTXOSpendingInfoFull
    with P2WSHV0SpendingInfo {

  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull = {
    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  scriptWitness.redeemScript,
                                  signersWithPossibleExtra,
                                  hashType,
                                  conditionalPath)
  }

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

/** This is the case where we are spending a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness v0 script]]  */
case class UnassignedSegwitNativeUTXOSpendingInfo(
    inputInfo: UnassignedSegwitNativeInputInfo,
    override val signers: Vector[Sign],
    hashType: HashType)
    extends BitcoinUTXOSpendingInfoFull
    with BitcoinUTXOSpendingInfoSingle {
  override val signer: Sign = signers.head

  override def scriptPubKey: WitnessScriptPubKey = inputInfo.scriptPubKey

  def scriptWitness: ScriptWitness = inputInfo.scriptWitness

  override val requiredSigs: Int = signers.length
}

sealed trait P2SHSpendingInfo extends BitcoinUTXOSpendingInfo {
  require(
    P2SHScriptPubKey(redeemScript) == output.scriptPubKey,
    s"Given redeem script did not match hash in output script, " +
      s"got=${P2SHScriptPubKey(redeemScript).scriptHash.hex}, " +
      s"expected=${scriptPubKey.scriptHash.hex}"
  )

  override def inputInfo: P2SHInputInfo
  override def scriptPubKey: P2SHScriptPubKey = inputInfo.scriptPubKey
  def redeemScript: ScriptPubKey = inputInfo.scriptPubKey
  def nestedSpendingInfo: BitcoinUTXOSpendingInfo
}

sealed trait P2SHSpendingInfoSingle
    extends P2SHSpendingInfo
    with BitcoinUTXOSpendingInfoSingle {
  def nestedSpendingInfo: BitcoinUTXOSpendingInfoSingle
}

sealed trait P2SHSpendingInfoFull
    extends BitcoinUTXOSpendingInfoFull
    with P2SHSpendingInfo {
  override def nestedSpendingInfo: BitcoinUTXOSpendingInfoFull
}

sealed trait P2SHNoNestSpendingInfo extends P2SHSpendingInfo {
  override def inputInfo: P2SHNoNestInputInfo
  override def redeemScript: RawScriptPubKey = inputInfo.redeemScript

  def nestedSpendingInfo: RawScriptUTXOSpendingInfo
}

case class P2SHNoNestSpendingInfoSingle(
    inputInfo: P2SHNoNestInputInfo,
    signer: Sign,
    hashType: HashType)
    extends P2SHNoNestSpendingInfo
    with P2SHSpendingInfoSingle {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoSingle = {
    RawScriptUTXOSpendingInfoSingle(outPoint,
                                    amount,
                                    redeemScript,
                                    signer,
                                    hashType,
                                    conditionalPath)
  }
}

/** This is the case were we are attempting to spend a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNoNestSpendingInfoFull(
    inputInfo: P2SHNoNestInputInfo,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType)
    extends P2SHSpendingInfoFull
    with P2SHNoNestSpendingInfo {
  override val nestedSpendingInfo: RawScriptUTXOSpendingInfoFull =
    RawScriptUTXOSpendingInfoFull(outPoint,
                                  amount,
                                  redeemScript,
                                  signersWithPossibleExtra,
                                  hashType,
                                  conditionalPath)

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}

sealed trait P2SHNestedSegwitV0UTXOSpendingInfo extends P2SHSpendingInfo {
  require(
    isValidScriptWitness(redeemScript, scriptWitness),
    s"Invalid ScriptWitness for redeem script: $scriptWitness - $redeemScript")

  override def inputInfo: P2SHNestedSegwitV0InputInfo
  override def redeemScript: WitnessScriptPubKeyV0 = inputInfo.redeemScript
  def scriptWitness: ScriptWitnessV0 = inputInfo.scriptWitness
  def nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfo
}

case class P2SHNestedSegwitV0UTXOSpendingInfoSingle(
    inputInfo: P2SHNestedSegwitV0InputInfo,
    signer: Sign,
    hashType: HashType)
    extends P2SHNestedSegwitV0UTXOSpendingInfo
    with P2SHSpendingInfoSingle {
  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfoSingle =
    SegwitV0NativeUTXOSpendingInfoSingle(outPoint,
                                         amount,
                                         redeemScript,
                                         signer,
                                         hashType,
                                         scriptWitness,
                                         conditionalPath)
}

/** This is for the case we are spending a p2sh(p2w{pkh,sh}) script. This means that
  * we have nested a [[org.bitcoins.core.protocol.script.WitnessScriptPubKeyV0 witness spk]]
  * inside of a [[org.bitcoins.core.protocol.script.P2SHScriptPubKey p2sh spk]] */
case class P2SHNestedSegwitV0UTXOSpendingInfoFull(
    inputInfo: P2SHNestedSegwitV0InputInfo,
    private val signersWithPossibleExtra: Vector[Sign],
    hashType: HashType)
    extends P2SHSpendingInfo
    with P2SHNestedSegwitV0UTXOSpendingInfo
    with P2SHSpendingInfoFull {
  override val nestedSpendingInfo: SegwitV0NativeUTXOSpendingInfoFull =
    SegwitV0NativeUTXOSpendingInfoFull(outPoint,
                                       amount,
                                       redeemScript,
                                       signersWithPossibleExtra,
                                       hashType,
                                       scriptWitness,
                                       conditionalPath)

  override val signers: Vector[Sign] = nestedSpendingInfo.signers

  override val requiredSigs: Int = nestedSpendingInfo.requiredSigs
}
