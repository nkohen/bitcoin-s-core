package org.bitcoins.dlc.data

import org.bitcoins.core.protocol.dlc.DLCMessage.{DLCAccept, DLCOffer, DLCSign}
import org.bitcoins.core.protocol.dlc.data.InMemoryDLCDataStore
import org.bitcoins.testkitcore.gen.{NumberGenerator, TLVGen}
import org.bitcoins.testkitcore.util.BitcoinSUnitTest

class DLCDataStoreTest extends BitcoinSUnitTest {
  behavior of "InMemoryDLCDataStore"

  it should "correctly write and read an offer" in {
    forAll(TLVGen.dlcOfferTLV) { offerTLV =>
      val offer = DLCOffer.fromTLV(offerTLV)

      val dataStore = InMemoryDLCDataStore()
      assert(dataStore.offerOpt.isEmpty)
      assertThrows[RuntimeException](dataStore.getter.getOffer)

      dataStore.writeOffer(offer)
      assert(dataStore.getter.getOffer == offer)
      assert(dataStore.offerOpt.contains(offer))
    }
  }

  it should "correctly write and read an accept" in {
    forAll(TLVGen.dlcOfferTLVAcceptTLV, NumberGenerator.bool) {
      case ((offerTLV, acceptTLV), isInitiator) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)

        val dataStore = InMemoryDLCDataStore()
        assert(dataStore.acceptWithoutSigsOpt.isEmpty)
        assert(dataStore.acceptOpt.isEmpty)
        assertThrows[RuntimeException](dataStore.getter.getAcceptWithoutSigs)
        assertThrows[RuntimeException](dataStore.getter.getAccept)

        dataStore.local.setIsInitiator(isInitiator)
        dataStore.global.setContractInfo(offer.contractInfo)
        assert(dataStore.acceptWithoutSigsOpt.isEmpty)
        assert(dataStore.acceptOpt.isEmpty)
        assertThrows[RuntimeException](dataStore.getter.getAcceptWithoutSigs)
        assertThrows[RuntimeException](dataStore.getter.getAccept)

        dataStore.writeAccept(accept)
        assert(dataStore.getter.getAcceptWithoutSigs == accept.withoutSigs)
        assert(dataStore.getter.getAccept == accept)
        assert(dataStore.acceptWithoutSigsOpt.contains(accept.withoutSigs))
        assert(dataStore.acceptOpt.contains(accept))
    }
  }

  it should "correctly write and read a sign" in {
    forAll(TLVGen.dlcOfferTLVAcceptTLVSignTLV, NumberGenerator.bool) {
      case ((offerTLV, _, signTLV), isInitiator) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val dataStore = InMemoryDLCDataStore()
        assert(dataStore.signOpt.isEmpty)
        assertThrows[RuntimeException](dataStore.getter.getSign)

        dataStore.local.setIsInitiator(isInitiator)
        dataStore.global.setContractInfo(offer.contractInfo)
        assert(dataStore.signOpt.isEmpty)
        assertThrows[RuntimeException](dataStore.getter.getSign)

        dataStore.writeSign(sign)
        assert(dataStore.getter.getSign == sign)
        assert(dataStore.signOpt.contains(sign))
    }
  }
}
