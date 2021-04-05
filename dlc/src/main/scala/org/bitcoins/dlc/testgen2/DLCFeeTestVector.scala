package org.bitcoins.dlc.testgen2

import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.wallet.builder.{
  DualFundingInput,
  DualFundingTxFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.ECPublicKey
import scodec.bits.ByteVector
import ujson.{Num, Value}
import upickle.default._

case class DLCFeeTestVector(
    inputs: DLCFeeTestVectorInput,
    offerFundingFee: Satoshis,
    offerClosingFee: Satoshis,
    acceptFundingFee: Satoshis,
    acceptClosingFee: Satoshis)
    extends TestVector {

  override def toJson: Value = {
    writeJs(this)(DLCFeeTestVector.dlcFeeTestVectorFormat)
  }
}

case class FundingFeeInfo(redeemScriptLen: Int, maxWitnessLen: Int) {

  lazy val mockDualFundingInput: DualFundingInput = {
    val scriptSig = if (redeemScriptLen == 0) {
      EmptyScriptSignature
    } else {
      val wspk = if (redeemScriptLen == 22) {
        P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
      } else {
        P2WSHWitnessSPKV0(EmptyScriptPubKey)
      }
      P2SHScriptSignature(wspk)
    }

    DualFundingInput(scriptSig, maxWitnessLen)
  }
}

case class DLCFeeTestVectorInput(
    offerInputs: Vector[FundingFeeInfo],
    offerPayoutSPKLen: Int,
    offerChangeSPKLen: Int,
    acceptInputs: Vector[FundingFeeInfo],
    acceptPayoutSPKLen: Int,
    acceptChangeSPKLen: Int,
    feeRate: SatoshisPerVirtualByte) {

  lazy val mockDualFundingTxFinalizer: DualFundingTxFinalizer = {
    def mockSPK(len: Int): ScriptPubKey = {
      ScriptPubKey.fromAsmBytes(ByteVector.fill(len)(0x00.toByte))
    }

    DualFundingTxFinalizer(
      offerInputs.map(_.mockDualFundingInput),
      mockSPK(offerPayoutSPKLen),
      mockSPK(offerChangeSPKLen),
      acceptInputs.map(_.mockDualFundingInput),
      mockSPK(acceptPayoutSPKLen),
      mockSPK(acceptChangeSPKLen),
      feeRate,
      EmptyScriptPubKey
    )
  }

  lazy val offerFundingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.offerFundingFee

  lazy val offerClosingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.offerFutureFee

  lazy val acceptFundingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.acceptFundingFee

  lazy val acceptClosingFee: CurrencyUnit =
    mockDualFundingTxFinalizer.acceptFutureFee
}

object DLCFeeTestVectorInput {

  def fromJson(json: Value): DLCFeeTestVectorInput = {
    read[DLCFeeTestVectorInput](json)(
      DLCFeeTestVector.dlcFeeTestVectorInputFormat)
  }
}

object DLCFeeTestVector extends TestVectorParser[DLCFeeTestVector] {

  def apply(inputs: DLCFeeTestVectorInput): DLCFeeTestVector = {
    DLCFeeTestVector(
      inputs,
      inputs.offerFundingFee.satoshis,
      inputs.offerClosingFee.satoshis,
      inputs.acceptFundingFee.satoshis,
      inputs.acceptClosingFee.satoshis
    )
  }

  def apply(
      offerInputs: Vector[FundingFeeInfo],
      offerPayoutSPKLen: Int,
      offerChangeSPKLen: Int,
      acceptInputs: Vector[FundingFeeInfo],
      acceptPayoutSPKLen: Int,
      acceptChangeSPKLen: Int,
      feeRate: SatoshisPerVirtualByte): DLCFeeTestVector = {
    DLCFeeTestVector(
      DLCFeeTestVectorInput(
        offerInputs,
        offerPayoutSPKLen,
        offerChangeSPKLen,
        acceptInputs,
        acceptPayoutSPKLen,
        acceptChangeSPKLen,
        feeRate
      )
    )
  }

  implicit val fundingFeeInfoFormat: ReadWriter[FundingFeeInfo] =
    macroRW[FundingFeeInfo]

  implicit val satsPerVBFormat: ReadWriter[SatoshisPerVirtualByte] =
    readwriter[Value].bimap[SatoshisPerVirtualByte](
      { satsPerVB => Num(satsPerVB.toLong.toDouble) },
      { json =>
        SatoshisPerVirtualByte(Satoshis(read[Long](json)))
      }
    )

  implicit val dlcFeeTestVectorInputFormat: ReadWriter[DLCFeeTestVectorInput] =
    macroRW[DLCFeeTestVectorInput]

  implicit val satoshisFormat: ReadWriter[Satoshis] =
    readwriter[Value].bimap[Satoshis](
      { sats => Num(sats.toLong.toDouble) },
      { json => Satoshis(read[Long](json)) }
    )

  implicit val dlcFeeTestVectorFormat: ReadWriter[DLCFeeTestVector] =
    macroRW[DLCFeeTestVector]

  override def fromJson(json: Value): DLCFeeTestVector = {
    read[DLCFeeTestVector](json)
  }
}
