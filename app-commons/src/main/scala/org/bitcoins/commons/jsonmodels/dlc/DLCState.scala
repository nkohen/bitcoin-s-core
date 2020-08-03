package org.bitcoins.commons.jsonmodels.dlc

sealed abstract class DLCState

object DLCState {

  /** The state where an offer has been created but no
    * accept message has yet been created/received.
    */
  final case object Offered extends DLCState

  /** The state where an offer has been accepted but
    * no sign message has yet been created/received.
    */
  final case object Accepted extends DLCState

  /** The state where the initiating party has created
    * a sign message in response to an accept message
    * but the DLC funding transaction has not yet been
    * broadcasted to the network.
    */
  final case object Signed extends DLCState

  /** The state where the accepting (non-initiating)
    * party has broadcasted the DLC funding transaction
    * to the blockchain, and it has not yet been confirmed.
    */
  final case object Broadcasted extends DLCState

  /** The state where the DLC funding transaction has been
    * confirmed on-chain and no execution paths have yet been
    * initiated.
    */
  final case object Confirmed extends DLCState

  /** The state where one of the CETs has been accepted by the network
    * and executed by ourselves.
    */
  final case object Claimed extends DLCState

  /** The state where one of the CETs has been accepted by the network
    * and executed by a remote party.
    */
  final case object RemoteClaimed extends DLCState

  /** The state where the DLC refund transaction has been
    * accepted by the network.
    */
  final case object Refunded extends DLCState

  val all: Vector[DLCState] = Vector(Offered,
                                     Accepted,
                                     Signed,
                                     Broadcasted,
                                     Confirmed,
                                     Claimed,
                                     RemoteClaimed,
                                     Refunded)

  def fromString(str: String): Option[DLCState] = {
    all.find(state => str.toLowerCase() == state.toString.toLowerCase)
  }
}
