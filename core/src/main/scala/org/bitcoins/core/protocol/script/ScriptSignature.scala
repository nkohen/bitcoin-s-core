package org.bitcoins.core.protocol.script

import org.bitcoins.core.crypto.{ECDigitalSignature, ECPublicKey}
import org.bitcoins.core.script.constant._
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.util._

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 12/26/15.
  *
  */
sealed abstract class ScriptSignature extends Script {

  /**
    * The digital signatures contained inside of the script signature
    * p2pkh script signatures only have one sig
    * p2pk script signatures only have one sigs
    * p2sh script signatures can have m sigs
    * multisignature scripts can have m sigs
    */
  def signatures: Seq[ECDigitalSignature]

}

case class NonStandardScriptSignature(override val asm: Vector[ScriptToken])
    extends ScriptSignature {
  def signatures: Seq[ECDigitalSignature] = Nil
  override def toString: String = "NonStandardScriptSignature(" + hex + ")"
}

object NonStandardScriptSignature
    extends ScriptFactory[NonStandardScriptSignature] {

  override def fromAsm(asm: Seq[ScriptToken]): NonStandardScriptSignature = {
    buildScript(asm = asm.toVector,
                constructor = NonStandardScriptSignature.apply,
                invariant = trivialInvariant,
                errorMsg = "")
  }
}

/**
  * P2PKH script signatures have only one public key
  * https://bitcoin.org/en/developer-guide#pay-to-public-key-hash-p2pkh
  * P2PKH scriptSigs follow this format
  * <sig> <pubkey>
  */
case class P2PKHScriptSignature(
    signature: ECDigitalSignature,
    publicKey: ECPublicKey)
    extends ScriptSignature {

  override val asm: Vector[ScriptToken] = {
    val signatureBytesToPushOntoStack =
      BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val pubKeyBytesToPushOntoStack =
      BitcoinScriptUtil.calculatePushOp(publicKey.bytes)
    val tokens: Seq[ScriptToken] =
      signatureBytesToPushOntoStack ++
        Seq(ScriptConstant(signature.hex)) ++
        pubKeyBytesToPushOntoStack ++
        Seq(ScriptConstant(publicKey.hex))

    tokens.toVector
  }

  override def signatures: Seq[ECDigitalSignature] = {
    Vector(signature)
  }

  override def toString: String = "P2PKHScriptSignature(" + hex + ")"

}

object P2PKHScriptSignature extends ScriptFactory[P2PKHScriptSignature] {

  override def fromAsm(asm: Seq[ScriptToken]): P2PKHScriptSignature = {
    if (isP2PKHScriptSig(asm)) {
      P2PKHScriptSignature(signature = ECDigitalSignature(asm(1).hex),
                           publicKey = ECPublicKey(asm.last.bytes))
    } else {
      throw new IllegalArgumentException(
        s"Given asm was not a P2PKHScriptSignature, got: $asm")
    }
  }

  /** Determines if the given asm matches a [[P2PKHScriptSignature]] */
  def isP2PKHScriptSig(asm: Seq[ScriptToken]): Boolean = asm match {
    case Seq(_: BytesToPushOntoStack,
             _: ScriptConstant,
             _: BytesToPushOntoStack,
             z: ScriptConstant) =>
      if (ECPublicKey.isFullyValid(z.bytes)) true
      else !P2SHScriptSignature.isRedeemScript(z)
    case _ => false
  }
}

/**
  * Represents a pay-to-script-hash script signature
  * https://bitcoin.org/en/developer-guide#pay-to-script-hash-p2sh
  * P2SH scriptSigs have the following format
  * <sig> [sig] [sig...] <redeemScript>
  */
case class P2SHScriptSignature(override val asm: Vector[ScriptToken])
    extends ScriptSignature {

  /** The redeemScript represents the conditions that must be satisfied to spend the output */
  def redeemScript: ScriptPubKey = {
    val scriptSig = scriptSignatureNoRedeemScript
    if (scriptSig == EmptyScriptSignature &&
        WitnessScriptPubKey.isWitnessScriptPubKey(asm.tail)) {
      //if we have an EmptyScriptSignature, we need to check if the rest of the asm
      //is a Witness script. It is not necessarily a witness script, since this code
      //path might be used for signing a normal p2sh spk in TransactionSignatureSerializer
      WitnessScriptPubKey(asm.tail).get
    } else {
      ScriptPubKey.fromAsmBytes(asm.last.bytes)
    }

  }

  /** Returns the script signature of this p2shScriptSig with no serialized redeemScript */
  def scriptSignatureNoRedeemScript: ScriptSignature = {
    //witness scriptPubKeys always have EmptyScriptSigs
    if (WitnessScriptPubKey.isWitnessScriptPubKey(asm)) {
      EmptyScriptSignature
    } else {
      val asmWithoutRedeemScriptAndPushOp: Try[Seq[ScriptToken]] = Try {
        asm(asm.size - 2) match {
          case _: BytesToPushOntoStack => asm.dropRight(2)
          case _                       => asm.dropRight(3)
        }
      }
      val script =
        asmWithoutRedeemScriptAndPushOp.getOrElse(EmptyScriptSignature.asm)
      ScriptSignature.fromAsm(script)
    }
  }

  /** Returns the public keys for the p2sh scriptSignature */
  def publicKeys: Seq[ECPublicKey] = {
    val pubKeys: Seq[ScriptToken] = redeemScript.asm
      .filter(_.isInstanceOf[ScriptConstant])
      .filterNot(_.isInstanceOf[ScriptNumberOperation])
    pubKeys.map(k => ECPublicKey(k.hex))
  }

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    val sigs = {
      scriptSignatureNoRedeemScript.asm
        .filter(_.isInstanceOf[ScriptConstant])
        .filterNot(_.isInstanceOf[ScriptNumberOperation])
        .filterNot(_.hex.length < 100)
    }
    sigs.map(s => ECDigitalSignature(s.hex))
  }

  /**
    * Splits the given asm into two parts
    * the first part is the digital signatures
    * the second part is the redeem script
    */
  def splitAtRedeemScript: (Seq[ScriptToken], Seq[ScriptToken]) = {
    (scriptSignatureNoRedeemScript.asm, redeemScript.asm)
  }

  override def toString: String = "P2SHScriptSignature(" + hex + ")"
}

object P2SHScriptSignature extends ScriptFactory[P2SHScriptSignature] {

  def apply(
      scriptSig: ScriptSignature,
      redeemScript: ScriptPubKey): P2SHScriptSignature = {
    //we need to calculate the size of the redeemScript and add the corresponding push op
    val serializedRedeemScript = ScriptConstant(redeemScript.asmBytes)
    val pushOps = BitcoinScriptUtil.calculatePushOp(serializedRedeemScript)
    val asm: Seq[ScriptToken] = scriptSig.asm ++ pushOps ++ Seq(
      serializedRedeemScript)
    fromAsm(asm)
  }

  def apply(witnessScriptPubKey: WitnessScriptPubKey): P2SHScriptSignature = {
    P2SHScriptSignature(EmptyScriptSignature, witnessScriptPubKey)
  }

  override def fromAsm(asm: Seq[ScriptToken]): P2SHScriptSignature = {
    //everything can be a P2SHScriptSignature, thus passing the trivially true function
    //the most important thing to note is we cannot have a P2SHScriptSignature unless
    //we have a P2SHScriptPubKey
    //previously P2SHScriptSignature's redeem script had to be standard scriptPubKey's, this
    //was removed in 0.11 or 0.12 of Bitcoin Core
    buildScript(asm = asm.toVector,
                constructor = P2SHScriptSignature.apply,
                invariant = trivialInvariant,
                errorMsg =
                  s"Given asm tokens are not a p2sh scriptSig, got: $asm")
  }

  /** Tests if the given asm tokens are a [[P2SHScriptSignature]] */
  def isP2SHScriptSig(asm: Seq[ScriptToken]): Boolean = asm match {
    case _ if asm.size > 1 && isRedeemScript(asm.last)       => true
    case _ if WitnessScriptPubKey.isWitnessScriptPubKey(asm) => true
    case _                                                   => false
  }

  /** Detects if the given script token is a redeem script */
  def isRedeemScript(token: ScriptToken): Boolean = {
    val redeemScriptTry: Try[ScriptPubKey] = parseRedeemScript(token)
    redeemScriptTry match {
      case Success(redeemScript) =>
        redeemScript match {
          case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey |
              _: P2SHScriptPubKey | _: P2PKScriptPubKey | _: CLTVScriptPubKey |
              _: CSVScriptPubKey | _: WitnessScriptPubKeyV0 |
              _: UnassignedWitnessScriptPubKey =>
            true
          case _: NonStandardScriptPubKey | _: WitnessCommitment => false
          case EmptyScriptPubKey                                 => false
        }
      case Failure(_) => false
    }
  }

  /** Parses a redeem script from the given script token */
  def parseRedeemScript(scriptToken: ScriptToken): Try[ScriptPubKey] = {
    val asm = ScriptParser.fromBytes(scriptToken.bytes)
    val redeemScript: Try[ScriptPubKey] = Try(ScriptPubKey(asm))
    redeemScript
  }
}

/**
  * Represents a multisignature script signature
  * https://bitcoin.org/en/developer-guide#multisig
  * Multisig script sigs have the following format
  * OP_0 <A sig> [B sig] [C sig...]
  */
case class MultiSignatureScriptSignature(override val asm: Vector[ScriptToken])
    extends ScriptSignature {

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    asm.tail
      .filter(_.isInstanceOf[ScriptConstant])
      .map(sig => ECDigitalSignature(sig.hex))
  }

  override def toString: String = "MultiSignatureScriptSignature(" + hex + ")"
}

object MultiSignatureScriptSignature
    extends ScriptFactory[MultiSignatureScriptSignature] {

  def apply(
      signatures: Seq[ECDigitalSignature]): MultiSignatureScriptSignature = {
    val sigsPushOpsPairs: Seq[Seq[ScriptToken]] = for {
      sig <- signatures
      constant = ScriptConstant(sig.bytes)
      pushOps = BitcoinScriptUtil.calculatePushOp(sig.bytes)
    } yield pushOps ++ Seq(constant)
    val sigsWithPushOps = sigsPushOpsPairs.flatten
    //OP_0 is for the dummy input required by OP_CHECKMULTISIG
    val asm = OP_0 +: sigsWithPushOps
    MultiSignatureScriptSignature.fromAsm(asm)
  }

  override def fromAsm(asm: Seq[ScriptToken]): MultiSignatureScriptSignature = {
    buildScript(
      asm = asm.toVector,
      constructor = MultiSignatureScriptSignature.apply,
      invariant = isMultiSignatureScriptSignature,
      errorMsg =
        s"The given asm tokens were not a multisignature script sig: $asm"
    )
  }

  /**
    * Checks if the given script tokens are a multisignature script sig
    * format: OP_0 <A sig> [B sig] [C sig...]
    *
    * @param asm the asm to check if it falls in the multisignature script sig format
    * @return boolean indicating if the scriptsignature is a multisignature script signature
    */
  def isMultiSignatureScriptSignature(asm: Seq[ScriptToken]): Boolean =
    if (asm.isEmpty) {
      false
    } else {
      val firstTokenIsScriptNumberOperation =
        asm.head.isInstanceOf[ScriptNumberOperation]
      val restOfScriptIsPushOpsOrScriptConstants = !asm.tail.forall(
        token =>
          token.isInstanceOf[ScriptConstant] || StackPushOperationFactory
            .isPushOperation(token))
      firstTokenIsScriptNumberOperation && !restOfScriptIsPushOpsOrScriptConstants
    }
}

/**
  * Represents a pay to public key script signature
  * https://bitcoin.org/en/developer-guide#pubkey
  * Signature script: <sig>
  */
case class P2PKScriptSignature(override val asm: Vector[ScriptToken])
    extends ScriptSignature {

  /** PubKey scriptSignatures only have one signature */
  def signature: ECDigitalSignature = signatures.head

  /** The digital signatures inside of the scriptSig */
  def signatures: Seq[ECDigitalSignature] = {
    Seq(ECDigitalSignature(BitcoinScriptUtil.filterPushOps(asm).head.hex))
  }

  override def toString: String = s"P2PKScriptSignature($hex)"
}

object P2PKScriptSignature extends ScriptFactory[P2PKScriptSignature] {

  def apply(signature: ECDigitalSignature): P2PKScriptSignature = {
    val pushOps = BitcoinScriptUtil.calculatePushOp(signature.bytes)
    val signatureConstant = ScriptConstant(signature.bytes)
    val asm = pushOps ++ Seq(signatureConstant)
    P2PKScriptSignature.fromAsm(asm)
  }

  override def fromAsm(asm: Seq[ScriptToken]): P2PKScriptSignature = {
    buildScript(asm.toVector,
                P2PKScriptSignature.apply,
                isP2PKScriptSignature,
                "The given asm tokens were not a p2pk script sig: " + asm)
  }

  /** P2PK scriptSigs always have the pattern [pushop, digitalSignature] */
  def isP2PKScriptSignature(asm: Seq[ScriptToken]): Boolean = asm match {
    case Seq(_: BytesToPushOntoStack, _: ScriptConstant) => true
    case _                                               => false
  }
}

/** Parent type for all lock time script signatures, these spend [[LockTimeScriptPubKey]] */
sealed trait LockTimeScriptSignature extends ScriptSignature {
  def scriptSig: ScriptSignature = ScriptSignature(hex)

  override def signatures: Seq[ECDigitalSignature] = scriptSig.signatures
}

case class CLTVScriptSignature(override val asm: Vector[ScriptToken])
    extends LockTimeScriptSignature {
  override def toString: String = s"CLTVScriptSignature($hex)"
}

/**
  * Note that extend [[org.bitcoins.core.protocol.script.ScriptFactory]] here
  * but technically ANYTHING can be a [[CLTVScriptSignature]] since the
  * [[CLTVScriptPubKey]] does not manipulate the stack
  */
object CLTVScriptSignature extends ScriptFactory[CLTVScriptSignature] {

  override def fromAsm(asm: Seq[ScriptToken]): CLTVScriptSignature = {
    buildScript(asm = asm.toVector,
                constructor = CLTVScriptSignature.apply,
                invariant = trivialInvariant,
                errorMsg = s"Given asm was not a CLTVScriptSignature $asm")
  }

  override def fromHex(hex: String): CLTVScriptSignature = {
    CLTVScriptSignature(BitcoinSUtil.decodeHex(hex))
  }

  def apply(scriptSig: ScriptSignature): CLTVScriptSignature = {
    fromHex(scriptSig.hex)
  }
}

case class CSVScriptSignature(override val asm: Vector[ScriptToken])
    extends LockTimeScriptSignature {
  override def toString: String = s"CSVScriptSignature($hex)"
}

object CSVScriptSignature extends ScriptFactory[CSVScriptSignature] {

  override def fromAsm(asm: Seq[ScriptToken]): CSVScriptSignature = {
    buildScript(asm = asm.toVector,
                constructor = CSVScriptSignature.apply,
                invariant = trivialInvariant,
                errorMsg = s"Given asm was not a CLTVScriptSignature $asm")
  }

  override def fromHex(hex: String): CSVScriptSignature = {
    CSVScriptSignature(BitcoinSUtil.decodeHex(hex))
  }

  def apply(scriptSig: ScriptSignature): CSVScriptSignature = {
    fromHex(scriptSig.hex)
  }
}

/** Represents the empty script signature */
case object EmptyScriptSignature extends ScriptSignature {
  override def asm: Seq[ScriptToken] = Vector.empty
  override def signatures: Seq[ECDigitalSignature] = Vector.empty
}

object ScriptSignature extends ScriptFactory[ScriptSignature] {

  /** Returns an empty script signature */
  def empty: ScriptSignature = EmptyScriptSignature

  /** Creates a scriptSignature from the list of script tokens */
  override def fromAsm(tokens: Seq[ScriptToken]): ScriptSignature = {
    if (tokens.isEmpty) {
      EmptyScriptSignature
    } else if (tokens.size > 1 && P2SHScriptSignature.isRedeemScript(
                 tokens.last)) {
      P2SHScriptSignature.fromAsm(tokens)
    } else if (MultiSignatureScriptSignature.isMultiSignatureScriptSignature(
                 tokens)) {
      MultiSignatureScriptSignature.fromAsm(tokens)
    } else if (P2PKHScriptSignature.isP2PKHScriptSig(tokens)) {
      P2PKHScriptSignature.fromAsm(tokens)
    } else if (P2PKScriptSignature.isP2PKScriptSignature(tokens)) {
      P2PKScriptSignature.fromAsm(tokens)
    } else {
      NonStandardScriptSignature.fromAsm(tokens)
    }
  }

  /**
    * Creates a script signature from the given tokens and scriptPubKey
    * @param tokens the script signature's tokens
    * @param scriptPubKey the scriptPubKey which the script signature is trying to spend
    * @return
    */
  @tailrec
  def fromScriptPubKey(
      tokens: Seq[ScriptToken],
      scriptPubKey: ScriptPubKey): Try[ScriptSignature] = scriptPubKey match {
    case _: P2SHScriptPubKey  => Try(P2SHScriptSignature.fromAsm(tokens))
    case _: P2PKHScriptPubKey => Try(P2PKHScriptSignature.fromAsm(tokens))
    case _: P2PKScriptPubKey  => Try(P2PKScriptSignature.fromAsm(tokens))
    case _: MultiSignatureScriptPubKey =>
      Try(MultiSignatureScriptSignature.fromAsm(tokens))
    case _: NonStandardScriptPubKey =>
      Try(NonStandardScriptSignature.fromAsm(tokens))
    case s: CLTVScriptPubKey => fromScriptPubKey(tokens, s.nestedScriptPubKey)
    case s: CSVScriptPubKey  => fromScriptPubKey(tokens, s.nestedScriptPubKey)
    case _: WitnessScriptPubKeyV0 | _: UnassignedWitnessScriptPubKey =>
      Success(EmptyScriptSignature)
    case EmptyScriptPubKey =>
      if (tokens.isEmpty) Success(EmptyScriptSignature)
      else Try(NonStandardScriptSignature.fromAsm(tokens))
    case _: WitnessCommitment =>
      Failure(
        new IllegalArgumentException(
          "Cannot spend witness commitment scriptPubKey"))
  }

  def apply(
      tokens: Seq[ScriptToken],
      scriptPubKey: ScriptPubKey): Try[ScriptSignature] = {
    fromScriptPubKey(tokens, scriptPubKey)
  }
}
