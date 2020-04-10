package org.bitcoins.core.crypto

import org.bitcoins.core.protocol.NetworkElement
import org.bitcoins.core.util.Factory
import scodec.bits.ByteVector

case class SchnorrDigitalSignature(rx: SchnorrNonce, sig: FieldElement)
    extends NetworkElement {
  override def bytes: ByteVector = rx.bytes ++ sig.bytes
}

object SchnorrDigitalSignature extends Factory[SchnorrDigitalSignature] {
  override def fromBytes(bytes: ByteVector): SchnorrDigitalSignature = {
    SchnorrDigitalSignature(SchnorrNonce(bytes.take(32)),
                            FieldElement(bytes.drop(32)))
  }
}
