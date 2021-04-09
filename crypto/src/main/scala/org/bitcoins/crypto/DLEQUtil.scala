package org.bitcoins.crypto

import scodec.bits.ByteVector

object DLEQUtil {

  def dleqPair(
      fe: FieldElement,
      adaptorPoint: ECPublicKey): (ECPublicKey, ECPublicKey) = {
    val point = fe.getPublicKey
    val tweakedPoint = adaptorPoint.tweakMultiply(fe)

    (point, tweakedPoint)
  }

  def dleqNonceFunc(
      fe: FieldElement,
      adaptorPoint: ECPublicKey,
      point: ECPublicKey,
      tweakedPoint: ECPublicKey,
      auxRand: ByteVector): FieldElement = {
    val hash = CryptoUtil
      .sha256(point.compressed.bytes ++ tweakedPoint.compressed.bytes)
      .bytes

    AdaptorUtil.adaptorNonce(hash,
                             fe.toPrivateKey,
                             adaptorPoint,
                             "DLEQ",
                             auxRand)
  }

  def dleqChallengeHash(
      adaptorPoint: ECPublicKey,
      r1: ECPublicKey,
      r2: ECPublicKey,
      p1: ECPublicKey,
      p2: ECPublicKey): ByteVector = {
    CryptoUtil
      .sha256DLEQ(
        p1.compressed.bytes ++ adaptorPoint.compressed.bytes ++
          p2.compressed.bytes ++ r1.compressed.bytes ++ r2.compressed.bytes)
      .bytes
  }

  /** Proves that the DLOG_G(R') = DLOG_Y(R) (= fe)
    * For a full description, see https://cs.nyu.edu/courses/spring07/G22.3220-001/lec3.pdf
    */
  def dleqProve(
      fe: FieldElement,
      adaptorPoint: ECPublicKey,
      auxRand: ByteVector): (FieldElement, FieldElement) = {
    // (fe*G, fe*Y)
    val (p1, p2) = dleqPair(fe, adaptorPoint)

    val k = dleqNonceFunc(fe, adaptorPoint, p1, p2, auxRand)

    if (k.isZero) {
      throw new RuntimeException("Nonce cannot be zero.")
    }

    val (r1, r2) = dleqPair(k, adaptorPoint)

    // Hash all components to get a challenge (this is the trick that turns
    // interactive ZKPs into non-interactive ZKPs, using hash assumptions)
    //
    // In short, rather than having the verifier present challenges, hash
    // all shared information (so that both parties can compute) and use
    // this hash as the challenge to the prover as loosely speaking this
    // should only be game-able if the prover can reverse hash functions.
    val challengeHash =
      dleqChallengeHash(adaptorPoint, r1, r2, p1, p2)
    val e = FieldElement(challengeHash)

    // s = k + fe*challenge. This proof works because then k = fe*challenge - s
    // so that R' = k*G =?= p1*challenge - s and R = k*Y =?= p2*challenge - s
    // can both be verified given s and challenge and will be true if and only
    // if R = y*R' which is what we are trying to prove.
    val s = fe.multiply(e).add(k)

    (e, s)
  }

  /** Verifies a proof that the DLOG_G of P1 equals the DLOG_adaptor of P2 */
  def dleqVerify(
      s: FieldElement,
      e: FieldElement,
      p1: ECPublicKey,
      adaptor: ECPublicKey,
      p2: ECPublicKey): Boolean = {
    val r1 = p1.tweakMultiply(e.negate).add(s.getPublicKey)
    val r2 = p2.tweakMultiply(e.negate).add(adaptor.tweakMultiply(s))
    val challengeHash = dleqChallengeHash(adaptor, r1, r2, p1, p2)

    challengeHash == e.bytes
  }

}
