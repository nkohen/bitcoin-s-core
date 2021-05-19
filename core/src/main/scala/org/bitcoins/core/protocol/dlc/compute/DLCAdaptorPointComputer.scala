package org.bitcoins.core.protocol.dlc.compute

import org.bitcoins.core.protocol.dlc.models.{
  ContractInfo,
  EnumContractDescriptor,
  NumericContractDescriptor
}
import org.bitcoins.core.protocol.tlv.{
  EnumOutcome,
  SignedNumericOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto.{
  CryptoUtil,
  ECPublicKey,
  FieldElement,
  SchnorrPublicKey
}
import scodec.bits.ByteVector

/** Responsible for optimized computation of DLC adaptor point batches. */
object DLCAdaptorPointComputer {

  private val base: Int = 2

  private lazy val numericPossibleOutcomes: Vector[ByteVector] = {
    0
      .until(base)
      .toVector
      .map(_.toString)
      .map(CryptoUtil.serializeForHash)
  }

  /** Computes:
    *     nonce + outcomeHash*pubKey
    * where outcomeHash is as specified in the DLC spec.
    * @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Oracle.md#signing-algorithm
    */
  def computePoint(
      pubKey: SchnorrPublicKey,
      nonce: ECPublicKey,
      outcome: ByteVector): ECPublicKey = {
    val hash = CryptoUtil
      .sha256SchnorrChallenge(
        nonce.schnorrNonce.bytes ++ pubKey.bytes ++ CryptoUtil
          .sha256DLCAttestation(outcome)
          .bytes)
      .bytes

    nonce.add(pubKey.publicKey.tweakMultiply(FieldElement(hash)))
  }

  /** Efficiently computes all adaptor points, in order, for a given ContractInfo.
    * @see https://medium.com/crypto-garage/optimizing-numeric-outcome-dlc-creation-6d6091ac0e47
    */
  def computeAdaptorPoints(contractInfo: ContractInfo): Vector[ECPublicKey] = {
    // The possible messages a single nonce may be used to sign
    val possibleOutcomes: Vector[ByteVector] =
      contractInfo.contractDescriptor match {
        case enum: EnumContractDescriptor =>
          enum.keys.map(_.outcome).map(CryptoUtil.serializeForHash)
        case _: NumericContractDescriptor => numericPossibleOutcomes
      }

    // Oracle -> Nonce -> Outcome -> SubSigPoint
    // These are the points that are then combined to construct aggregate points.
    val preComputeTable: Vector[Vector[Vector[ECPublicKey]]] =
      contractInfo.oracleInfo.singleOracleInfos.map { info =>
        val announcement = info.announcement
        val pubKey = announcement.publicKey
        val nonces = announcement.eventTLV.nonces.map(_.publicKey)

        nonces.map { nonce =>
          possibleOutcomes.map { outcome =>
            computePoint(pubKey, nonce, outcome)
          }
        }
      }

    // Oracle -> Tree Index -> Adaptor Point
    val sigPointTree: Vector[Vector[ECPublicKey]] =
      contractInfo.contractDescriptor match {
        case _: EnumContractDescriptor =>
          preComputeTable.map(_.head)
        case _: NumericContractDescriptor =>
          preComputeTable.map { preComputeForOracle =>
            val inputArr = preComputeForOracle.flatten
              .map(_.decompressedBytes.toArray)
              .toArray
            val outputArr =
              org.bitcoin.NativeSecp256k1.pubKeyTableMemoize(inputArr)
            outputArr.toVector.map(ByteVector.apply).map(ECPublicKey.fromBytes)
          }
      }

    val oraclesAndOutcomes = contractInfo.allOutcomes.map(_.oraclesAndOutcomes)

    oraclesAndOutcomes.map { oracleAndOutcome =>
      val oracleSigPoints = oracleAndOutcome.map { case (info, outcome) =>
        val oracleIndex =
          contractInfo.oracleInfo.singleOracleInfos.indexOf(info)
        val outcomeIndex = outcome match {
          case outcome: EnumOutcome =>
            contractInfo.contractDescriptor
              .asInstanceOf[EnumContractDescriptor]
              .keys
              .indexOf(outcome)
          case UnsignedNumericOutcome(digits) =>
            val indexInRow =
              NumberUtil.fromDigits(digits, base, digits.length).toInt
            (1 << digits.length) - 2 + indexInRow
          case _: SignedNumericOutcome =>
            throw new UnsupportedOperationException(
              "Signed numeric outcomes not supported!")
        }

        sigPointTree(oracleIndex)(outcomeIndex)
      }

      CryptoUtil.combinePubKeys(oracleSigPoints)
    }
  }
}
