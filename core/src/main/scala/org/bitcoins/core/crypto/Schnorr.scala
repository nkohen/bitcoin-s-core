package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoin.NativeSecp256k1
import org.bitcoins.core.util.CryptoUtil
import scodec.bits.ByteVector

object Schnorr { // TODO: TEST Against the actual binary (which you have to build)

  val NONCE_TAG: ByteVector = ByteVector(
    "BIP340/nonce".toCharArray.map(_.toByte))
  val NONCE_HASH_TAG: ByteVector = CryptoUtil.sha256(NONCE_TAG).bytes

  val CHALLENGE_TAG: ByteVector = ByteVector(
    "BIP340/challenge".toCharArray.map(_.toByte))
  val CHALLENGE_HASH_TAG: ByteVector = CryptoUtil.sha256(CHALLENGE_TAG).bytes

  /** Generates a Schnorr signature for the 32 byte msg using privateKey */
  def sign(
      msg: ByteVector,
      privateKey: ECPrivateKey): SchnorrDigitalSignature = {
    signWithNonce(msg, privateKey, SchnorrNonce.fromBipSchnorr(privateKey, msg))
  }

  /** Generates a Schnorr signature for the 32 byte msg using privateKey
    * and nonce.
    *
    * ```IMPORTANT```: Never sign two messages with the same privateKey and nonce!
    *                  This leaks your private key publicly.
    */
  def signWithNonce(
      msg: ByteVector,
      privateKey: ECPrivateKey,
      nonce: SchnorrNonce): SchnorrDigitalSignature = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    // TODO: More validation

    val correctedPrivKey =
      if (new BigInteger(
            1,
            privateKey.publicKey.toPoint.getRawXCoord.getEncoded).testBit(0)) {
        privateKey.negate
      } else {
        privateKey
      }
    val rx = ByteVector(nonce.publicKey.toPoint.getRawXCoord.getEncoded)
    val pkx = ByteVector(
      correctedPrivKey.publicKey.toPoint.getRawXCoord.getEncoded)

    val e = new BigInteger(
      1,
      CryptoUtil
        .sha256(CHALLENGE_HASH_TAG ++ CHALLENGE_HASH_TAG ++ rx ++ pkx ++ msg)
        .bytes
        .toArray)
    val s = e
      .multiply(new BigInteger(1, correctedPrivKey.bytes.toArray))
      .add(new BigInteger(1, nonce.bytes.toArray))
      .mod(CryptoParams.curve.getN)

    SchnorrDigitalSignature.fromRxS(rx, ByteVector(s.toByteArray))
  }

  /** Verifies a Schnorr signature of a given msg with a given publicKey */
  def verify(
      msg: ByteVector,
      sig: SchnorrDigitalSignature,
      publicKey: ECPublicKey): Boolean = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    // TODO: More Validation

    val pkx = ByteVector(publicKey.toPoint.getRawXCoord.getEncoded)
    val e =
      CryptoUtil
        .sha256(
          CHALLENGE_HASH_TAG ++ CHALLENGE_HASH_TAG ++ sig.rx ++ pkx ++ msg)
        .bytes
    val negE = ECPrivateKey(e).negate.toBigInteger
    val expectedR = publicKey.toPoint
      .multiply(negE)
      .add(ECPrivateKey(sig.s).publicKey.toPoint)

    ByteVector(expectedR.getRawXCoord.getEncoded) == sig.rx
  }

  /** Computes the public key associated with Schnorr signature from public information */
  def computePubKey(
      msg: ByteVector,
      r: ECPublicKey,
      publicKey: ECPublicKey): ECPublicKey = {
    require(msg.length == 32, s"Message must be 32 bytes, got $msg")

    // TODO: More Validation

    val pkx = ByteVector(publicKey.toPoint.getRawXCoord.getEncoded)
    val rx = ByteVector(r.toPoint.getRawXCoord.getEncoded)
    val e = CryptoUtil
      .sha256(CHALLENGE_HASH_TAG ++ CHALLENGE_HASH_TAG ++ rx ++ pkx ++ msg)
      .bytes
    val sigPoint = publicKey.toPoint
      .multiply(new BigInteger(1, e.toArray))
      .add(r.toPoint)

    ECPublicKey.fromPoint(sigPoint)
  }

  /** Computes the public key associated with a SchnorrNonce as specified in bip-schnorr.
    * They y-coordinate is chosen to be a quadratic residue.
    * @see [[https://github.com/sipa/bips/blob/bip-schnorr/bip-schnorr.mediawiki#design]]
    */
  def computeR(nonce: SchnorrNonce): ECPublicKey = {
    // TODO: More Validation

    if (nonce.toPrivKey.publicKey.toPoint.getRawYCoord.sqrt() == null) {
      nonce.toPrivKey.negate.publicKey
    } else {
      nonce.toPrivKey.publicKey
    }
  }
}
