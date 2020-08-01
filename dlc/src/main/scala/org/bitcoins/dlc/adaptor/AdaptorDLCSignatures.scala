package org.bitcoins.dlc.adaptor

import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.MapWrapper
import org.bitcoins.crypto.{ECAdaptorSignature, Sha256DigestBE}

sealed trait AdaptorDLCSignatures

case class AdaptorFundingSignatures(
    sigs: Map[TransactionOutPoint, Vector[PartialSignature]])
    extends MapWrapper[TransactionOutPoint, Vector[PartialSignature]]
    with AdaptorDLCSignatures {

  override protected def wrapped: Map[
    TransactionOutPoint,
    Vector[PartialSignature]] = sigs

  def merge(other: AdaptorFundingSignatures): AdaptorFundingSignatures = {
    val outPoints = sigs.keys ++ other.keys
    val combinedSigs = outPoints.map { outPoint =>
      val thisSigs = sigs.get(outPoint).toVector.flatten
      val otherSigs = other.get(outPoint).toVector.flatten
      outPoint -> (thisSigs ++ otherSigs)
    }

    AdaptorFundingSignatures(combinedSigs.toMap)
  }
}

case class AdaptorCETSignatures(
    outcomeSigs: Map[Sha256DigestBE, ECAdaptorSignature],
    refundSig: PartialSignature)
    extends AdaptorDLCSignatures
