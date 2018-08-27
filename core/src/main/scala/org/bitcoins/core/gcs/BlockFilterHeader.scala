package org.bitcoins.core.gcs

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

/**
 * Filter headers are similar to block headers in the fact that they commit to
 * the previous [[BlockFilter]] they are building on top of
 * [[https://github.com/bitcoin/bips/blob/master/bip-0157.mediawiki#filter-headers]]
 */
sealed abstract class BlockFilterHeader extends NetworkElement {

  /** The hash of the [[BlockFilter]] we are committing to */
  def hash: DoubleSha256Digest

  /** The previous [[BlockFilter]] hash we are building on top of */
  def prevHash: DoubleSha256Digest

  override def bytes: ByteVector = {
    prevHash.bytes ++ hash.bytes
  }

}

object BlockFilterHeader extends Factory[BlockFilterHeader] {
  private case class BlockFilterHeaderImpl(prevHash: DoubleSha256Digest, hash: DoubleSha256Digest) extends BlockFilterHeader

  override def fromBytes(bytes: ByteVector): BlockFilterHeader = {
    val prevHash = DoubleSha256Digest(bytes.take(32))
    val hash = DoubleSha256Digest(bytes.slice(32, 64))
    BlockFilterHeaderImpl(prevHash, hash)
  }

}
