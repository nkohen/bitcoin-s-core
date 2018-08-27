package org.bitcoins.core.gcs

import org.bitcoins.core.crypto.{ DoubleSha256Digest, HashDigest }
import org.bitcoins.core.protocol.blockchain.{ Block, BlockHeader }
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.slf4j.LoggerFactory
import play.api.libs.json.{ JsArray, Json }

case class GCSTestCase(
  height: Int,
  hash: DoubleSha256Digest,
  block: Block,
  prevOutScripts: Vector[ScriptPubKey],
  prevHeader: BlockFilterHeader,
  filter: BlockFilter,
  header: BlockFilterHeader)

object GCSTestCase {
  private val logger = LoggerFactory.getLogger(this.getClass.getSimpleName)
  //["Block Height,Block Hash,Block,[Prev Output Scripts for Block],Previous Basic Header,Basic Filter,Basic Header,Notes"]
  def fromJsArray(array: JsArray): GCSTestCase = {
    val height = array(0).validate[Int].get
    val blockHash = DoubleSha256Digest.fromHex(array(1).validate[String].get)

    val block = Block.fromHex(array(2).validate[String].get)

    val scriptArray = array(3).validate[JsArray].get
    val scripts = parseScripts(scriptArray)

    val prevHeader = BlockFilterHeader.fromHex(array(4).validate[String].get)

    val filter = BlockFilter.fromHex(array(5).validate[String].get)

    val header = BlockFilterHeader.fromHex(array(6).validate[String].get)

    GCSTestCase(
      height = height,
      hash = blockHash,
      block = block,
      prevOutScripts = scripts,
      prevHeader = prevHeader,
      filter = filter,
      header = header)
  }

  private def parseScripts(array: JsArray): Vector[ScriptPubKey] = {
    val hexScripts = array.value.map(js => js.validate[String].get)

    val spks = hexScripts.map(hex => ScriptPubKey.fromHex(hex))

    spks.toVector
  }
}
