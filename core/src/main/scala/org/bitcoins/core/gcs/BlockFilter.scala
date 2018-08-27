package org.bitcoins.core.gcs

import org.bitcoins.core.protocol.{ CompactSizeUInt, NetworkElement }
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.core.script.control.OP_RETURN
import org.bitcoins.core.util.Factory
import org.bitcoins.core.wallet.utxo.UTXOSpendingInfo
import scodec.bits.ByteVector

import scala.collection.mutable.ArrayBuffer
import scala.util.Try

sealed abstract class BlockFilter extends NetworkElement {
  /** The number of elements in the [[GCS]] */
  def n: CompactSizeUInt

  def gcs: GCS

  override def bytes: ByteVector = {
    val b = ArrayBuffer.newBuilder[Byte]
    n.bytes.foreach(byte => b.+=(byte))

    gcs.bytes.foreach(byte => b.+=(byte))

    ByteVector(b.result())
  }

}

object BlockFilter extends Factory[BlockFilter] {

  private case class BlockFilterImpl(n: CompactSizeUInt, gcs: GCS) extends BlockFilter

  def apply(n: CompactSizeUInt, gcs: GCS): BlockFilter = {
    BlockFilterImpl(n = n, gcs = gcs)
  }

  override def fromBytes(bytes: ByteVector): BlockFilter = {

    val n = CompactSizeUInt.parse(bytes)
    val gcs = GCS.fromBytes(bytes.splitAt(n.size)._2)
    BlockFilterImpl(n, gcs)
  }

  /**
   * Constructs a [[BlockFilter]] from a block
   * [[https://github.com/bitcoin/bips/blob/master/bip-0158.mediawiki#contents]]
   * @param block
   * @param utxos
   * @return
   */
  def fromBlock(block: Block, utxos: Vector[UTXOSpendingInfo]): Try[BlockFilter] = {
    val key = block.blockHeader.hash.bytes.take(16)
    val noCoinbase = block.transactions.tail
    val createdOutputs = noCoinbase.flatMap(_.outputs)
    val spentSpks: Vector[ByteVector] = utxos.map(_.output.scriptPubKey.asmBytes)

    val createdSpks: Vector[ByteVector] = {
      val vecWithEmpties = {
        createdOutputs.map { output =>
          if (output.scriptPubKey.asm.contains(OP_RETURN)) {
            ByteVector.empty
          } else {
            output.scriptPubKey.asmBytes
          }
        }
      }

      vecWithEmpties.filter(_.nonEmpty)
    }.toVector

    val b = Vector.newBuilder[ByteVector]
    spentSpks.foreach(spk => b.+=(spk))
    createdSpks.foreach(spk => b.+=(spk))
    val data = b.result()
    val gcsT = GCS.build(
      key = key,
      data = data)

    gcsT.map(gcs => BlockFilter(???, gcs))
  }
}
