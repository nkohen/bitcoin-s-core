package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  DLCAccept,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.protocol.transaction.{Transaction, WitnessTransaction}
import org.bitcoins.crypto.{
  ECAdaptorSignature,
  ECDigitalSignature,
  SchnorrDigitalSignature,
  Sha256DigestBE
}

/** Represents the state of specific DLC for a given party.
  * This state is made up of all messages that have been
  * passed back and forth as well as any relevant on-chain
  * transactions and oracle signatures.
  */
sealed trait DLCStatus {
  def eventId: Sha256DigestBE
  def isInitiator: Boolean
  def offer: DLCOffer
  def statusString: String
}

/** All states other than Offered contain an accept message. */
sealed trait AcceptedDLCStatus extends DLCStatus {
  def accept: DLCAccept
}

object DLCStatus {

  /** The state where an offer has been created but no
    * accept message has yet been created/received.
    */
  case class Offered(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer)
      extends DLCStatus {
    override val statusString: String = "OFFERED"

    def toAccepted(accept: DLCAccept): Accepted = {
      Accepted(eventId, isInitiator, offer, accept)
    }
  }

  /** The state where an offer has been accepted but
    * no sign message has yet been created/received.
    */
  case class Accepted(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept)
      extends AcceptedDLCStatus {
    override val statusString: String = "ACCEPTED"

    def toSigned(sign: DLCSign): Signed = {
      Signed(eventId, isInitiator, offer, accept, sign)
    }
  }

  /** The state where the initiating party has created
    * a sign message in response to an accept message
    * but the DLC funding transaction has not yet been
    * broadcasted to the network.
    */
  case class Signed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign)
      extends AcceptedDLCStatus {
    override val statusString: String = "SIGNED"

    def toBroadcasted(fundingTx: Transaction): Broadcasted = {
      Broadcasted(eventId, isInitiator, offer, accept, sign, fundingTx)
    }

    def toConfirmed(fundingTx: Transaction): Confirmed = {
      toBroadcasted(fundingTx).toConfirmed
    }
  }

  /** The state where the accepting (non-initiating)
    * party has broadcasted the DLC funding transaction
    * to the blockchain, and it has not yet been confirmed.
    */
  case class Broadcasted(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "BROADCASTED"

    def toConfirmed: Confirmed = {
      Confirmed(eventId, isInitiator, offer, accept, sign, fundingTx)
    }
  }

  /** The state where the DLC funding transaction has been
    * confirmed on-chain and no execution paths have yet been
    * initiated.
    */
  case class Confirmed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "CONFIRMED"

    def toClaimed(
        oracleSig: SchnorrDigitalSignature,
        cet: Transaction): Claimed = {
      Claimed(eventId,
              isInitiator,
              offer,
              accept,
              sign,
              fundingTx,
              oracleSig,
              cet)
    }

    def toRemoteClaimed(cet: Transaction): RemoteClaimed = {
      RemoteClaimed(eventId, isInitiator, offer, accept, sign, fundingTx, cet)
    }
  }

  /** The state where this party has successfully executed
    * a unilateral execution by broadcasting a CET and successfully
    * spending the to_local output of that CET.
    */
  case class Claimed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      oracleSig: SchnorrDigitalSignature,
      cet: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "CLAIMED"
  }

  /** The state where the remote party has successfully
    * executed a unilateral execution by broadcasting both
    * a CET and successfully spending their to_local output.
    */
  case class RemoteClaimed(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      cet: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "REMOTE CLAIMED"

    val oracleSig: SchnorrDigitalSignature = {
      val cetSigs = cet
        .asInstanceOf[WitnessTransaction]
        .witness
        .head
        .asInstanceOf[P2WSHWitnessV0]
        .signatures
      val oraclePubKey = offer.oracleInfo.pubKey
      val preCommittedR = offer.oracleInfo.rValue

      def sigFromMsgAndSigs(
          msg: Sha256DigestBE,
          adaptorSig: ECAdaptorSignature,
          cetSig: ECDigitalSignature): SchnorrDigitalSignature = {
        val sigPubKey = oraclePubKey.computeSigPoint(msg.bytes, preCommittedR)
        val possibleOracleS =
          sigPubKey.extractAdaptorSecret(adaptorSig, cetSig).fieldElement
        SchnorrDigitalSignature(preCommittedR, possibleOracleS)
      }

      val (cetSig, outcomeSigs) = if (isInitiator) {
        val localValue = cet.outputs.head.value
        val possibleMessages =
          offer.contractInfo.filter(_._2 == localValue).keys
        val possibleOutcomeSigs = sign.cetSigs.outcomeSigs.filter {
          case (msg, _) => possibleMessages.exists(_ == msg)
        }
        (cetSigs.head, possibleOutcomeSigs)
      } else {
        val localValue = cet.outputs.last.value
        val localContractInfo = offer.contractInfo.map {
          case (msg, amt) =>
            msg -> (offer.totalCollateral + accept.totalCollateral - amt).satoshis
        }
        val possibleMessages = localContractInfo.filter(_._2 == localValue).keys
        val possibleOutcomeSigs = sign.cetSigs.outcomeSigs.filter {
          case (msg, _) => possibleMessages.exists(_ == msg)
        }
        (cetSigs.last, possibleOutcomeSigs)
      }

      val sigOpt = outcomeSigs.find {
        case (msg, adaptorSig) =>
          val possibleOracleSig = sigFromMsgAndSigs(msg, adaptorSig, cetSig)
          oraclePubKey.verify(msg.bytes, possibleOracleSig)
      }

      sigOpt match {
        case Some((msg, adaptorSig)) =>
          sigFromMsgAndSigs(msg, adaptorSig, cetSig)
        case None =>
          throw new IllegalArgumentException(
            "No Oracle Siganture found from CET")
      }
    }
  }

  /** The state where the DLC refund transaction has been
    * accepted by the network.
    */
  case class Refunded(
      eventId: Sha256DigestBE,
      isInitiator: Boolean,
      offer: DLCOffer,
      accept: DLCAccept,
      sign: DLCSign,
      fundingTx: Transaction,
      refundTx: Transaction)
      extends AcceptedDLCStatus {
    override val statusString: String = "REFUNDED"
  }
}
