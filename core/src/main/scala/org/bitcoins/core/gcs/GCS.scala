package org.bitcoins.core.gcs

import org.bitcoins.core.number.{ UInt32, UInt64, UInt8 }
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import org.bouncycastle.crypto.macs.SipHash
import org.bouncycastle.crypto.params.KeyParameter
import scodec.bits.{ BitVector, ByteVector }

import scala.annotation.tailrec
import scala.util.{ Failure, Success, Try }

/**
 * Represents a Golomb-Coded Sets which is used as the data structure for Light Client's in BIP158
 * The exact specification can be found here
 * [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#GolombCoded_Sets]]
 */
sealed abstract class GCS extends NetworkElement {
  require(k.size == 128, s"The key used to randomize SipHash outputs must be 128 bits, got ${k.size}")

  /** The bit parameter for the GCS */
  def p: UInt8

  /** Target false positive rate */
  def m: UInt64

  /** The 128 bit key used to randomize siphash outputs */
  def k: ByteVector

}

object GCS extends Factory[GCS] {
  private case class GCSImpl(p: UInt8, m: UInt64, k: ByteVector, bytes: ByteVector) extends GCS

  //https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#construction
  val P: UInt8 = UInt8(19)
  val M: UInt64 = UInt64(784931)

  def apply(p: UInt8, m: UInt64, k: ByteVector, bytes: ByteVector): GCS = {
    GCSImpl(p, m, k, bytes)
  }

  override def fromBytes(bytes: ByteVector): GCS = ???

  /**
   * Mimics this function for building a GCSFilter
   * [[https://github.com/Roasbeef/btcutil/blob/b5d74480bb5b02a15a9266cbeae37ecf9dd6ffca/gcs/gcs.go#L56]]
   *
   * @param p the bit parameter of the Golomb-Rice coding
   * @param key the 128-bit key used to randomize the SipHash outputs
   * @param data items encoded in the GCS
   * @param m the target false positive rate
   * @return
   */
  def build(p: UInt8, m: UInt64, key: ByteVector, data: Vector[ByteVector]): Try[GCS] = {
    if (data.isEmpty) {
      Failure(new IllegalArgumentException(s"Cannot create a GCS filter with an empty data set"))
    } else if (data.length > UInt32.max.toInt) {
      Failure(new IllegalArgumentException(s"Cannot create a GCS filter because the filter is too big ${data.length}"))
    } else if (p > UInt8(32)) {
      Failure(new IllegalArgumentException(s"Cannot create a GCS filter because p was too large ${p}"))
    } else if (key.size != 16) {
      Failure(new IllegalArgumentException(s"Cannot create a GCS filter because the key was the wrong size, got ${key.size}"))
    } else {

      val hashedValues: Vector[UInt64] = constructHashedSet(data, key, m)

      val sorted = hashedValues.sortWith(_ <= _)

      val bitVector = buildBitVector(
        values = sorted,
        p = p)

      //note, there is implicit padding happening here
      //with the .toByteVector call, it pads to the nearest
      //byte boundary
      val gcs = GCS(
        p = p,
        m = m,
        k = key,
        bytes = bitVector.toByteVector)
      Success(gcs)
    }
  }

  /**
   * Builds a GCS with the default parameters specified by BIP158
   * [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#construction]]
   */
  def build(key: ByteVector, data: Vector[ByteVector]): Try[GCS] = {
    build(P, M, key, data)
  }

  private def constructHashedSet(data: Vector[ByteVector], key: ByteVector, m: UInt64): Vector[UInt64] = {
    val builder = Vector.newBuilder[UInt64]

    val n = UInt64(data.length)
    val f = m * n
    val hashedValuesBuilder = data.foldLeft(builder) {
      case (values, v) =>
        val u64 = hashToRange(v, f, key)
        values.+=(u64)
    }
    hashedValuesBuilder.result()
  }

  private def hashToRange(item: ByteVector, f: UInt64, key: ByteVector): UInt64 = {
    //return (siphash(k, item) * F) >> 64
    //params are 2,4 according to BIP158
    val sh = new SipHash(2, 4)

    val keyParam = new KeyParameter(key.toArray)

    sh.init(keyParam)

    sh.update(item.toArray, 0, item.length.toInt)

    val digest = new Array[Byte](8)

    sh.doFinal(digest, 0)

    val u64 = UInt64.fromBytes(ByteVector(digest))

    val bigInt = (u64.toBigInt * f.toBigInt) >> 64

    UInt64(bigInt)
  }

  private def buildBitVector(values: Vector[UInt64], p: UInt8): BitVector = {
    @tailrec
    def loop(remaining: Vector[UInt64], accum: BitVector, lastValue: UInt64): BitVector = {
      if (remaining.isEmpty) {
        accum
      } else {
        val item = remaining.head
        val delta = item - lastValue
        val encoded = GCS.encode(delta, p)
        loop(remaining.tail, accum ++ encoded, item)
      }
    }
    loop(values, BitVector.empty, UInt64(p.toInt))
  }

  def encode(delta: UInt64, p: UInt8): BitVector = {
    val q = delta >> p.toInt

    @tailrec
    def loop(a: UInt64, accum: BitVector): BitVector = {
      if (a <= UInt64.zero) {
        accum
      } else {
        loop(a - UInt64.one, accum.:+(true))
      }
    }

    val set = loop(q, BitVector.empty)

    val append0 = set.:+(false)

    val x = delta

    //write_bits_big_endian(stream, n, k)</code>
    // appends the <code>k</code> least significant bits of integer <code>n</code>
    // to the end of the stream in big-endian bit order

    //write_bits_big_endian(stream, x, P)

    val end = x.bytes.toBitVector.takeRight(p.toInt)

    append0 ++ end
  }

  def decode(stream: BitVector, p: UInt8): UInt64 = {
    @tailrec
    def loop(inc: UInt64, vec: BitVector): (UInt64, BitVector) = {
      if (vec.nonEmpty && vec.head) {
        loop(inc + UInt64.one, vec.tail)
      } else {
        (inc, vec)
      }
    }

    val (q, noQuotient) = loop(UInt64.zero, stream)

    // <code>read_bits_big_endian(stream, k)</code> reads the next available
    // <code>k</code> bits from the stream and interprets them as the least
    // significant bits of a big-endian integer

    val rBytes = {
      //toByteVector defaults to padding on the
      //right most byte of the bytevector
      //we want to pad on the most significant byte
      val padding = if (p.toInt < 8) {
        8
      } else {
        (p.toInt % 8) + p.toInt
      }

      val noPadding = noQuotient.takeRight(p.toInt)

      noPadding.padLeft(padding).toByteVector
    }

    val r = UInt64.fromBytes(rBytes)
    val x = (q << p.toInt) + r

    x
  }

}
