package org.bitcoins.commons.jsonmodels.dlc

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{ExtPrivateKey, ExtPublicKey}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress}
import org.bitcoins.crypto.{ECPrivateKey, ECPublicKey}

case class DLCPublicKeys(fundingKey: ECPublicKey, payoutAddress: BitcoinAddress)

object DLCPublicKeys {

  def fromPrivKeys(
      fundingPrivKey: ECPrivateKey,
      payoutKey: ECPrivateKey,
      network: BitcoinNetwork): DLCPublicKeys = {
    DLCPublicKeys(
      fundingPrivKey.publicKey,
      Bech32Address(P2WPKHWitnessSPKV0(payoutKey.publicKey), network))
  }

  def fromExtPubKeyAndIndex(
      extPubKey: ExtPublicKey,
      nextAddressIndex: Int,
      network: BitcoinNetwork): DLCPublicKeys = {
    val fundingPubKey: ECPublicKey =
      extPubKey
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/$nextAddressIndex"))
        .get
        .key

    val payoutKey: ECPublicKey =
      extPubKey
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/${nextAddressIndex + 1}"))
        .get
        .key

    DLCPublicKeys(
      fundingKey = fundingPubKey,
      payoutAddress = Bech32Address(P2WPKHWitnessSPKV0(payoutKey), network)
    )
  }

  def fromExtPrivKeyAndIndex(
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      network: BitcoinNetwork): DLCPublicKeys = {
    fromExtPubKeyAndIndex(extPrivKey.extPublicKey, nextAddressIndex, network)
  }
}
