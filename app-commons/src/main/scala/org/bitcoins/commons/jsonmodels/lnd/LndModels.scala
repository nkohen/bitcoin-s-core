package org.bitcoins.commons.jsonmodels.lnd

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.ln.LnInvoice
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import scodec.bits.ByteVector

sealed abstract class LndModel

case class AddInvoiceResult(
    rHash: ByteVector,
    invoice: LnInvoice,
    addIndex: Long,
    paymentAddr: ByteVector)
    extends LndModel

case class UTXOResult(
    address: BitcoinAddress,
    amount: CurrencyUnit,
    spk: ScriptPubKey,
    outPointOpt: Option[TransactionOutPoint],
    confirmations: Long
) extends LndModel

case class WalletBalances(
    balance: CurrencyUnit,
    unconfirmedBalance: CurrencyUnit,
    confirmedBalance: CurrencyUnit
) extends LndModel

case class ChannelBalances(
    localBalance: CurrencyUnit,
    remoteBalance: CurrencyUnit,
    unsettledLocalBalance: CurrencyUnit,
    unsettledRemoteBalance: CurrencyUnit,
    pendingOpenLocalBalance: CurrencyUnit,
    pendingOpenRemoteBalance: CurrencyUnit
) extends LndModel