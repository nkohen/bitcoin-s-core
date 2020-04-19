package org.bitcoins.core.crypto

import java.math.BigInteger

import org.bitcoins.core.util.{BitcoinSUtil, CryptoUtil}
import org.bouncycastle.crypto.digests.SHA256Digest
import org.bouncycastle.crypto.params.{
  ECPrivateKeyParameters,
  ECPublicKeyParameters
}
import org.bouncycastle.crypto.signers.{ECDSASigner, HMacDSAKCalculator}
import org.bouncycastle.math.ec.{ECCurve, ECPoint}
import scodec.bits.ByteVector

import scala.util.Try

object BouncyCastleUtil {

  private val curve: ECCurve = CryptoParams.curve.getCurve
  private val G: ECPoint = CryptoParams.curve.getG
  private val N: BigInteger = CryptoParams.curve.getN

  private def getBigInteger(bytes: ByteVector): BigInteger = {
    new BigInteger(1, bytes.toArray)
  }

  def addNumbers(num1: ByteVector, num2: ByteVector): BigInteger = {
    val bigInteger1 = getBigInteger(num1)
    val bigInteger2 = getBigInteger(num2)

    bigInteger1.add(bigInteger2).mod(N)
  }

  def decodePoint(bytes: ByteVector): ECPoint = {
    curve.decodePoint(bytes.toArray)
  }

  def validatePublicKey(bytes: ByteVector): Boolean = {
    Try(decodePoint(bytes))
      .map(_.getCurve == curve)
      .getOrElse(false)
  }

  def pubKeyTweakMul(pubKey: ECPublicKey, tweak: FieldElement): ECPublicKey = {
    val tweakedPoint = pubKey.toPoint.multiply(tweak.toBigInteger)
    ECPublicKey.fromPoint(tweakedPoint, pubKey.isCompressed)
  }

  def decompressPublicKey(publicKey: ECPublicKey): ECPublicKey = {
    if (publicKey.isCompressed) {
      val point = decodePoint(publicKey.bytes)
      val decompressedBytes =
        ByteVector.fromHex("04").get ++
          ByteVector(point.getXCoord.getEncoded) ++
          ByteVector(point.getYCoord.getEncoded)
      ECPublicKey(decompressedBytes)
    } else publicKey
  }

  def computePublicKey(privateKey: ECPrivateKey): ECPublicKey = {
    val priv = getBigInteger(privateKey.bytes)
    val point = G.multiply(priv)
    val pubBytes = ByteVector(point.getEncoded(privateKey.isCompressed))
    require(
      ECPublicKey.isFullyValid(pubBytes),
      s"Bouncy Castle failed to generate a valid public key, got: ${BitcoinSUtil
        .encodeHex(pubBytes)}")
    ECPublicKey(pubBytes)
  }

  def sign(
      dataToSign: ByteVector,
      privateKey: ECPrivateKey): ECDigitalSignature = {
    val signer: ECDSASigner = new ECDSASigner(
      new HMacDSAKCalculator(new SHA256Digest()))
    val privKey: ECPrivateKeyParameters =
      new ECPrivateKeyParameters(getBigInteger(privateKey.bytes),
                                 CryptoParams.curve)
    signer.init(true, privKey)
    val components: Array[BigInteger] =
      signer.generateSignature(dataToSign.toArray)
    val (r, s) = (components(0), components(1))
    val signature = ECDigitalSignature(r, s)
    //make sure the signature follows BIP62's low-s value
    //https://github.com/bitcoin/bips/blob/master/bip-0062.mediawiki#Low_S_values_in_signatures
    //bitcoinj implementation
    //https://github.com/bitcoinj/bitcoinj/blob/1e66b9a8e38d9ad425507bf5f34d64c5d3d23bb8/core/src/main/java/org/bitcoinj/core/ECKey.java#L551
    val signatureLowS = DERSignatureUtil.lowS(signature)
    require(
      signatureLowS.isDEREncoded,
      "We must create DER encoded signatures when signing a piece of data, got: " + signatureLowS)
    signatureLowS
  }

  def verifyDigitalSignature(
      data: ByteVector,
      publicKey: ECPublicKey,
      signature: ECDigitalSignature): Boolean = {
    val resultTry = Try {
      val publicKeyParams =
        new ECPublicKeyParameters(decodePoint(publicKey.bytes),
                                  CryptoParams.curve)

      val signer = new ECDSASigner
      signer.init(false, publicKeyParams)
      signature match {
        case EmptyDigitalSignature =>
          signer.verifySignature(data.toArray,
                                 java.math.BigInteger.valueOf(0),
                                 java.math.BigInteger.valueOf(0))
        case _: ECDigitalSignature =>
          val rBigInteger: BigInteger = new BigInteger(signature.r.toString())
          val sBigInteger: BigInteger = new BigInteger(signature.s.toString())
          signer.verifySignature(data.toArray, rBigInteger, sBigInteger)
      }
    }
    resultTry.getOrElse(false)
  }
}

object AdaptorStuff {

  def serializePoint(point: ECPublicKey): ByteVector = {
    val (sign, xCoor) = point.bytes.splitAt(1)
    sign.map(b => (b & 0x01).toByte) ++ xCoor
  }

  def deserializePoint(point: ByteVector): ECPublicKey = {
    val (sign, xCoor) = point.splitAt(1)
    ECPublicKey(sign.map(b => (b | 0x02).toByte) ++ xCoor)
  }

  private def adaptorSignHelper(
      dataToSign: ByteVector,
      k: FieldElement,
      r: ECPublicKey,
      privateKey: ECPrivateKey): FieldElement = {
    val rx = FieldElement(r.toPoint.getXCoord.toBigInteger)
    val x = privateKey.fieldElement
    val m = FieldElement(dataToSign)
    val kInv = k.inverse

    rx.multiply(x).add(m).multiply(kInv)
  }

  def adaptorSign(
      privateKey: ECPrivateKey,
      adaptorPoint: ECPublicKey,
      dataToSign: ByteVector): ECAdaptorSignature = {
    val hash = CryptoUtil.sha256(dataToSign ++ serializePoint(adaptorPoint))
    val nonceBytes =
      CryptoUtil
        .taggedSha256(privateKey.bytes ++ hash.bytes, "ECDSAAdaptorNon")
        .bytes

    val k = FieldElement(nonceBytes)
    val untweakedNonce = k.getPublicKey
    val tweakedNonce = adaptorPoint.tweakMultiply(k)

    val (proofS, proofE) =
      DLEQStuff.dleqProve(k, adaptorPoint, "ECDSAAdaptorSig")

    val adaptedSig = adaptorSignHelper(dataToSign, k, tweakedNonce, privateKey)

    ECAdaptorSignature(
      serializePoint(tweakedNonce) ++ adaptedSig.bytes,
      serializePoint(untweakedNonce) ++ proofS.bytes ++ proofE.bytes)
  }

  def adaptorVerifyHelper(
      rx: FieldElement,
      s: FieldElement,
      pubKey: ECPublicKey,
      msg: ByteVector): FieldElement = {
    val m = FieldElement(msg)
    val untweakedPoint =
      m.getPublicKey.add(pubKey.tweakMultiply(rx)).tweakMultiply(s.inverse)

    FieldElement(untweakedPoint.bytes.tail)
  }

  def adaptorVerify(
      adaptorSig: ECAdaptorSignature,
      pubKey: ECPublicKey,
      data: ByteVector,
      adaptor: ECPublicKey): Boolean = {
    val untweakedNonce = deserializePoint(adaptorSig.dleqProof.take(33))
    val proofS = FieldElement(adaptorSig.dleqProof.drop(33).take(32))
    val proofR = FieldElement(adaptorSig.dleqProof.drop(65))

    val tweakedNonce = deserializePoint(adaptorSig.adaptedSig.take(33))
    val adaptedSig = FieldElement(adaptorSig.adaptedSig.drop(33))

    val validProof = DLEQStuff.dleqVerify(
      "ECDSAAdaptorSig",
      proofS,
      proofR,
      untweakedNonce,
      adaptor,
      tweakedNonce
    )

    if (validProof) {
      val untweakedRx = adaptorVerifyHelper(
        FieldElement(tweakedNonce.bytes.tail),
        adaptedSig,
        pubKey,
        data)

      untweakedRx == FieldElement(untweakedNonce.bytes.tail)
    } else {
      false
    }
  }

  def adaptorComplete(
      adaptorSecret: ECPrivateKey,
      adaptedSig: ByteVector): ECDigitalSignature = {
    ???
  }

  def extractAdaptorSecret(
      sig: ECDigitalSignature,
      adaptorSig: ECAdaptorSignature,
      adaptor: ECPublicKey): ECPrivateKey = {
    ???
  }
}

object DLEQStuff {
  import AdaptorStuff.serializePoint

  def dleqPair(
      fe: FieldElement,
      adaptorPoint: ECPublicKey): (ECPublicKey, ECPublicKey) = {
    val point = fe.getPublicKey
    val tweakedPoint = adaptorPoint.tweakMultiply(fe)

    (point, tweakedPoint)
  }

  def dleqNonceFunc(
      hash: ByteVector,
      fe: FieldElement,
      algoName: String): FieldElement = {
    val kBytes =
      CryptoUtil.taggedSha256(fe.bytes ++ hash, algoName).bytes
    FieldElement(kBytes)
  }

  def dleqChallengeHash(
      algoName: String,
      adaptorPoint: ECPublicKey,
      r1: ECPublicKey,
      r2: ECPublicKey,
      p1: ECPublicKey,
      p2: ECPublicKey): ByteVector = {
    CryptoUtil
      .taggedSha256(serializePoint(adaptorPoint) ++ serializePoint(r1) ++ serializePoint(
                      r2) ++ serializePoint(p1) ++ serializePoint(p2),
                    algoName)
      .bytes
  }

  def dleqProve(
      fe: FieldElement,
      adaptorPoint: ECPublicKey,
      algoName: String): (FieldElement, FieldElement) = {
    val (p1, p2) = dleqPair(fe, adaptorPoint)

    val hash =
      CryptoUtil
        .sha256(
          serializePoint(adaptorPoint) ++ serializePoint(p1) ++ serializePoint(
            p2))
        .bytes
    val k = dleqNonceFunc(hash, fe, algoName)

    val r1 = k.getPublicKey
    val r2 = adaptorPoint.tweakMultiply(k)

    val challengeHash =
      dleqChallengeHash(algoName, adaptorPoint, r1, r2, p1, p2)
    val e = FieldElement(challengeHash)
    val s = fe.multiply(e).add(k)

    (s, e)
  }

  def dleqVerify(
      algoName: String,
      s: FieldElement,
      e: FieldElement,
      p1: ECPublicKey,
      adaptor: ECPublicKey,
      p2: ECPublicKey): Boolean = {
    val r1 = p1.tweakMultiply(e.negate).add(s.getPublicKey)
    val r2 = p2.tweakMultiply(e.negate).add(adaptor.tweakMultiply(s))
    val challengeHash = dleqChallengeHash(algoName, adaptor, r1, r2, p1, p2)

    challengeHash == e.bytes
  }
}
