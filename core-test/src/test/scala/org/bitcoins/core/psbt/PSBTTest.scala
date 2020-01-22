package org.bitcoins.core.psbt

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.hd.BIP32Path
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.GlobalPSBTRecord.{UnsignedTransaction, Version}
import org.bitcoins.core.psbt.InputPSBTRecord.{
  NonWitnessOrUnknownUTXO,
  WitnessUTXO
}
import org.bitcoins.core.psbt.OutputPSBTRecord.{RedeemScript, WitnessScript}
import org.bitcoins.core.psbt.PSBTGlobalKeyId.XPubKeyKeyId
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfoFull,
  ConditionalPath
}
import org.bitcoins.testkit.core.gen._
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits._

import scala.concurrent.Future
import scala.util.{Failure, Success}

class PSBTTest extends BitcoinSAsyncTest {

  behavior of "PSBT"

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  it must "successfully create an empty PSBT" in {
    val tx = Transaction(
      "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000")

    val emptyPsbt = PSBT.fromUnsignedTx(tx)

    val expectedPsbt = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000000000000000000")

    assert(emptyPsbt == expectedPsbt)
  }

  it must "fail to create a PSBT of an unknown version" in {
    assertThrows[IllegalArgumentException](PSBT(
      "70736274ff01fb040e00000001009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000000000000000000"))
  }

  it must "correctly retrieve the version of a PSBT" in {
    val emptyPsbt = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000000000000000000")

    assert(emptyPsbt.version == UInt32.zero)

    val psbtWithVersion =
      PSBT(GlobalPSBTMap(emptyPsbt.globalMap.elements :+ Version(UInt32(14))),
           emptyPsbt.inputMaps,
           emptyPsbt.outputMaps)

    assert(psbtWithVersion.version == UInt32(14))
  }

  it must "correctly update a PSBT" in {

    val start = PSBT.fromBytes(
      hex"70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000000000000000000")

    val expected = PSBT(
      hex"70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e88701042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val extKey = ExtKey
      .fromString(
        "tprv8ZgxMBicQKsPd9TeAdPADNnSyH9SSUUbTVeFszDE23Ki6TBB5nCefAdHkK8Fm3qMQR6sHwA56zqRmKmxnHk37JkiFzvncDqoKmPWubu7hDF")
      .get

    val psbt = start
      .addUTXOToInput(
        Transaction(
          "0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f618765000000"),
        0
      )
      .addUTXOToInput(
        Transaction(
          "0200000000010158e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd7501000000171600145f275f436b09a8cc9a2eb2a2f528485c68a56323feffffff02d8231f1b0100000017a914aed962d6654f9a2b36608eb9d64d2b260db4f1118700c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e88702483045022100a22edcc6e5bc511af4cc4ae0de0fcd75c7e04d8c1c3a8aa9d820ed4b967384ec02200642963597b9b1bc22c75e9f3e117284a962188bf5e8a74c895089046a20ad770121035509a48eb623e10aace8bfd0212fdb8a8e5af3c94b0b133b95e114cab89e4f7965000000"),
        1
      )
      .addRedeemOrWitnessScriptToInput(
        ScriptPubKey.fromAsmBytes(
          hex"5221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae"),
        0
      )
      .addRedeemOrWitnessScriptToInput(
        ScriptPubKey.fromAsmBytes(
          hex"00208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903"),
        1)
      .addRedeemOrWitnessScriptToInput(
        ScriptPubKey.fromAsmBytes(
          hex"522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae"),
        1
      )
      .addKeyPathToInput(extKey, BIP32Path.fromString("m/0'/0'/0'"), 0)
      .addKeyPathToInput(extKey, BIP32Path.fromString("m/0'/0'/1'"), 0)
      .addKeyPathToInput(extKey, BIP32Path.fromString("m/0'/0'/2'"), 1)
      .addKeyPathToInput(extKey, BIP32Path.fromString("m/0'/0'/3'"), 1)
      .addKeyPathToOutput(extKey, BIP32Path.fromString("m/0'/0'/4'"), 0)
      .addKeyPathToOutput(extKey, BIP32Path.fromString("m/0'/0'/5'"), 1)

    assert(psbt == expected)

    val psbtWithSigHash = psbt
      .addSigHashTypeToInput(HashType.sigHashAll, 0)
      .addSigHashTypeToInput(HashType.sigHashAll, 1)

    val nextExpected = PSBT(
      hex"70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f618765000000010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8870103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    assert(psbtWithSigHash == nextExpected)
  }

  it must "fail to create a InputPSBTMap with both a NonWitness and Witness UTXO" in {
    val nonWitnessOrUnknownUTXO = NonWitnessOrUnknownUTXO(Transaction(
      "02000000019dfc6628c26c5899fe1bd3dc338665bfd55d7ada10f6220973df2d386dec12760100000000ffffffff01f03dcd1d000000001600147b3a00bfdc14d27795c2b74901d09da6ef13357900000000"))
    val witnessUTXO =
      WitnessUTXO(TransactionOutput(Satoshis.one, EmptyScriptPubKey))

    assertThrows[IllegalArgumentException](
      InputPSBTMap(Vector(nonWitnessOrUnknownUTXO, witnessUTXO)))
  }

  it must "correctly filter a GlobalPSBTMap" in {
    val psbt = PSBT(
      "70736274ff01009d0100000002710ea76ab45c5cb6438e607e59cc037626981805ae9e0dfd9089012abb0be5350100000000ffffffff190994d6a8b3c8c82ccbcfb2fba4106aa06639b872a8d447465c0d42588d6d670000000000ffffffff0200e1f505000000001976a914b6bc2c0ee5655a843d79afedd0ccc3f7dd64340988ac605af405000000001600141188ef8e4ce0449eaac8fb141cbf5a1176e6a088000000004f010488b21e039e530cac800000003dbc8a5c9769f031b17e77fea1518603221a18fd18f2b9a54c6c8c1ac75cbc3502f230584b155d1c7f1cd45120a653c48d650b431b67c5b2c13f27d7142037c1691027569c503100008000000080000000800001011f00e1f5050000000016001433b982f91b28f160c920b4ab95e58ce50dda3a4a220203309680f33c7de38ea6a47cd4ecd66f1f5a49747c6ffb8808ed09039243e3ad5c47304402202d704ced830c56a909344bd742b6852dccd103e963bae92d38e75254d2bb424502202d86c437195df46c0ceda084f2a291c3da2d64070f76bf9b90b195e7ef28f77201220603309680f33c7de38ea6a47cd4ecd66f1f5a49747c6ffb8808ed09039243e3ad5c1827569c5031000080000000800000008000000000010000000001011f00e1f50500000000160014388fb944307eb77ef45197d0b0b245e079f011de220202c777161f73d0b7c72b9ee7bde650293d13f095bc7656ad1f525da5fd2e10b11047304402204cb1fb5f869c942e0e26100576125439179ae88dca8a9dc3ba08f7953988faa60220521f49ca791c27d70e273c9b14616985909361e25be274ea200d7e08827e514d01220602c777161f73d0b7c72b9ee7bde650293d13f095bc7656ad1f525da5fd2e10b1101827569c5031000080000000800000008000000000000000000000220202d20ca502ee289686d21815bd43a80637b0698e1fbcdbe4caed445f6c1a0a90ef1827569c50310000800000008000000080000000000400000000")

    val records = psbt.globalMap.filterRecords(XPubKeyKeyId)
    assert(!records.exists(_.key.head == XPubKeyKeyId.byte))
    assert(
      GlobalPSBTMap(records).bytes == hex"01009d0100000002710ea76ab45c5cb6438e607e59cc037626981805ae9e0dfd9089012abb0be5350100000000ffffffff190994d6a8b3c8c82ccbcfb2fba4106aa06639b872a8d447465c0d42588d6d670000000000ffffffff0200e1f505000000001976a914b6bc2c0ee5655a843d79afedd0ccc3f7dd64340988ac605af405000000001600141188ef8e4ce0449eaac8fb141cbf5a1176e6a0880000000000")
  }

  it must "successfully combine two PSBTs" in {
    // PSBT with 2 inputs, and empty outputs
    val expected = PSBT.fromBytes(
      hex"70736274ff0100a00200000002ab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40000000000feffffffab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40100000000feffffff02603bea0b000000001976a914768a40bbd740cbe81d988e71de2a4d5c71396b1d88ac8e240000000000001976a9146f4620b553fa095e721b9ee0efe9fa039cca459788ac000000000001076a47304402204759661797c01b036b25928948686218347d89864b719e1f7fcf57d1e511658702205309eabf56aa4d8891ffd111fdf1336f3a29da866d7f8486d75546ceedaf93190121035cdc61fc7ba971c0b501a646a2a83b102cb43881217ca682dc86e2d73fa882920001012000e1f5050000000017a9143545e6e33b832c47050f24d3eeb93c9c03948bc787010416001485d13537f2e265405a34dbafa9e3dda01fb82308000000")
    val psbt1 =
      PSBT(expected.globalMap,
           Vector(expected.inputMaps.head, InputPSBTMap(Vector.empty)),
           expected.outputMaps)
    val psbt2 =
      PSBT(expected.globalMap,
           Vector(InputPSBTMap(Vector.empty), expected.inputMaps.last),
           expected.outputMaps)

    assert(psbt1.combinePSBT(psbt2) == expected)
    assert(psbt2.combinePSBT(psbt1) == expected)
    assert(psbt1.combinePSBT(psbt1) != expected)
    assert(psbt2.combinePSBT(psbt2) != expected)
  }

  it must "correctly combine PSBTs" in {
    forAllAsync(PSBTGenerators.arbitraryPSBT) { psbtF =>
      psbtF.map { psbt =>
        val global = psbt.globalMap
        val inputs = psbt.inputMaps
        val outputs = psbt.outputMaps

        val newGlobal = PSBTGenerators.pruneGlobal(global)
        val newInputs =
          inputs.map(input =>
            InputPSBTMap(PSBTGenerators.pruneVec(input.elements)))
        val newOutputs =
          outputs.map(output =>
            OutputPSBTMap(PSBTGenerators.pruneVec(output.elements)))

        val psbt1 = PSBT(newGlobal, newInputs, newOutputs)

        val oppositeGlobalElements = global.elements.filterNot(e =>
          newGlobal.elements.contains(e)) :+ global.unsignedTransaction
        val oppositeGlobal = GlobalPSBTMap(oppositeGlobalElements.distinct)
        val oppositeInputs = inputs.zip(newInputs).map {
          case (map, pruned) =>
            InputPSBTMap(
              map.elements.filterNot(e => pruned.elements.contains(e)))
        }
        val oppositeOutputs = outputs.zip(newOutputs).map {
          case (map, pruned) =>
            OutputPSBTMap(
              map.elements.filterNot(e => pruned.elements.contains(e)))
        }

        val psbt2 = PSBT(oppositeGlobal, oppositeInputs, oppositeOutputs)

        assert(psbt1.combinePSBT(psbt2) == psbt)
        assert(psbt2.combinePSBT(psbt1) == psbt)
      }
    }
  }

  it must "successfully combine two PSBTs with unknown types" in {
    val psbt1 = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f00")
    val psbt2 = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f00")
    val expected = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f00")

    assert(psbt1.combinePSBT(psbt2) == expected)
  }

  it must "correctly combine two GlobalPSBTMap with differing versions" in {
    val sharedTx = BaseTransaction(
      "02000000019dfc6628c26c5899fe1bd3dc338665bfd55d7ada10f6220973df2d386dec12760100000000ffffffff01f03dcd1d000000001600147b3a00bfdc14d27795c2b74901d09da6ef13357900000000")
    val unsignedTxRecord = UnsignedTransaction(sharedTx)

    val version0 = Version(UInt32.zero)
    val version1 = Version(UInt32.one)

    val globalMap0 = GlobalPSBTMap(Vector(unsignedTxRecord, version0))
    val globalMap1 = GlobalPSBTMap(Vector(unsignedTxRecord, version1))

    val combined = globalMap0.combine(globalMap1)

    assert(combined.version == version1)
  }

  it must "fail to combine two PSBTs with differing global transactions" in {
    val psbt0 = PSBT.fromUnsignedTx(Transaction(
      "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000"))

    val psbt1 = PSBT.fromUnsignedTx(Transaction(
      "02000000019dfc6628c26c5899fe1bd3dc338665bfd55d7ada10f6220973df2d386dec12760100000000ffffffff01f03dcd1d000000001600147b3a00bfdc14d27795c2b74901d09da6ef13357900000000"))

    assertThrows[IllegalArgumentException](psbt0.combinePSBT(psbt1))
    assertThrows[IllegalArgumentException](
      psbt0.globalMap.combine(psbt1.globalMap))
  }

  it must "correctly update PSBTs' inputs" in {
    forAllAsync(PSBTGenerators.psbtToBeSigned)(_.flatMap {
      case (fullPsbt, utxos) =>
        val emptyPsbt = PSBT.fromUnsignedTx(fullPsbt.transaction)

        val infoAndTxs = PSBTGenerators
          .spendingInfoAndNonWitnessTxsFromSpendingInfos(fullPsbt.transaction,
                                                         utxos.toVector)
          .infoAndTxOpts
        val updatedPSBT = infoAndTxs.zipWithIndex.foldLeft(emptyPsbt) {
          case (psbt, ((utxo, txOpt), index)) =>
            val partUpdatedPsbt = psbt
              .addUTXOToInput(txOpt.get, index)
              .addSigHashTypeToInput(utxo.hashType, index)

            (utxo.redeemScriptOpt, utxo.scriptWitnessOpt) match {
              case (Some(redeemScript), Some(scriptWitness)) =>
                partUpdatedPsbt
                  .addRedeemOrWitnessScriptToInput(redeemScript, index)
                  .addScriptWitnessToInput(scriptWitness, index)
              case (Some(redeemScript), None) =>
                partUpdatedPsbt
                  .addRedeemOrWitnessScriptToInput(redeemScript, index)
              case (None, Some(scriptWitness)) =>
                partUpdatedPsbt
                  .addScriptWitnessToInput(scriptWitness, index)
              case (None, None) =>
                partUpdatedPsbt
            }

        }
        assert(updatedPSBT == fullPsbt)
    })
  }

  it must "successfully extract a transaction from a finalized PSBT" in {
    val psbt = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000000107da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae0001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8870107232200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b20289030108da0400473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d20147522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae00220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")
    val expected = Transaction.fromHex(
      "0200000000010258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd7500000000da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752aeffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d01000000232200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000400473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d20147522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae00000000")

    val txT = psbt.extractTransactionAndValidate

    assert(txT.isSuccess)

    val tx = psbt.extractTransaction

    assert(tx == txT.get)
    assert(tx == expected)
  }

  it must "finalize an already finalized input" in {
    val inputMap = InputPSBTMap(
      "0x0100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000000107da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae00")
    val finalizedT = inputMap.finalize(EmptyTransactionInput)
    assert(finalizedT.isSuccess)
    assert(inputMap == finalizedT.get)
  }

  it must "fail to finalize an input with no UTXO" in {
    val inputMap = InputPSBTMap(Vector.empty)
    val finalizedT = inputMap.finalize(EmptyTransactionInput)

    assert(finalizedT.isFailure)
  }

  it must "fail to extract a transaction from a non-finalized PSBT" in {
    val psbt = PSBT(
      "70736274ff0100a00200000002ab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40000000000feffffffab0949a08c5af7c49b8212f417e2f15ab3f5c33dcf153821a8139f877a5b7be40100000000feffffff02603bea0b000000001976a914768a40bbd740cbe81d988e71de2a4d5c71396b1d88ac8e240000000000001976a9146f4620b553fa095e721b9ee0efe9fa039cca459788ac000000000001076a47304402204759661797c01b036b25928948686218347d89864b719e1f7fcf57d1e511658702205309eabf56aa4d8891ffd111fdf1336f3a29da866d7f8486d75546ceedaf93190121035cdc61fc7ba971c0b501a646a2a83b102cb43881217ca682dc86e2d73fa882920001012000e1f5050000000017a9143545e6e33b832c47050f24d3eeb93c9c03948bc787010416001485d13537f2e265405a34dbafa9e3dda01fb82308000000")

    assertThrows[IllegalStateException](psbt.extractTransaction)
  }

  it must "successfully finalize a PSBT" in {
    val psbt = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000002202029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01220202dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d7483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e887220203089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f012202023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e73473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d2010103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val expected = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000000107da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae0001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8870107232200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b20289030108da0400473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d20147522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae00220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val finalizedPSBT = psbt.finalizePSBT
    assert(finalizedPSBT.isSuccess)
    assert(finalizedPSBT.get == expected)
  }

  private def getDummySigners(
      size: Int,
      pubKey: ECPublicKey = ECPublicKey.freshPublicKey): Vector[Sign] = {
    Vector.fill(size)(Sign.dummySign(pubKey))
  }

  it must "create a valid UTXOSpendingInfo" in {
    // PSBT with one P2WSH input of a 2-of-2 multisig. witnessScript, keypaths, and global xpubs are available. Contains no signatures. Outputs filled.
    val psbt = PSBT(
      "70736274ff01005202000000019dfc6628c26c5899fe1bd3dc338665bfd55d7ada10f6220973df2d386dec12760100000000ffffffff01f03dcd1d000000001600147b3a00bfdc14d27795c2b74901d09da6ef133579000000004f01043587cf02da3fd0088000000097048b1ad0445b1ec8275517727c87b4e4ebc18a203ffa0f94c01566bd38e9000351b743887ee1d40dc32a6043724f2d6459b3b5a4d73daec8fbae0472f3bc43e20cd90c6a4fae000080000000804f01043587cf02da3fd00880000001b90452427139cd78c2cff2444be353cd58605e3e513285e528b407fae3f6173503d30a5e97c8adbc557dac2ad9a7e39c1722ebac69e668b6f2667cc1d671c83cab0cd90c6a4fae000080010000800001012b0065cd1d000000002200202c5486126c4978079a814e13715d65f36459e4d6ccaded266d0508645bafa6320105475221029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c88712103372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b52ae2206029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c887110d90c6a4fae0000800000008000000000220603372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b10d90c6a4fae0000800100008000000000002202039eff1f547a1d5f92dfa2ba7af6ac971a4bd03ba4a734b03156a256b8ad3a1ef910ede45cc500000080000000800100008000")
    val dummySigners = getDummySigners(2)
    val spendingInfo =
      psbt.getUTXOSpendingInfoUsingSigners(index = 0, dummySigners)

    assert(spendingInfo.outPoint == psbt.transaction.inputs.head.previousOutput)
    assert(spendingInfo.amount == Satoshis(500000000))
    assert(
      spendingInfo.scriptPubKey == P2WSHWitnessSPKV0.fromHash(Sha256Digest(
        "2c5486126c4978079a814e13715d65f36459e4d6ccaded266d0508645bafa632")))
    assert(spendingInfo.signers == dummySigners)
    assert(spendingInfo.hashType == HashType.sigHashAll)
    assert(spendingInfo.redeemScriptOpt.isEmpty)
    assert(
      spendingInfo.scriptWitnessOpt.contains(
        P2WSHWitnessV0(RawScriptPubKey.fromAsmHex(
          "5221029da12cdb5b235692b91536afefe5c91c3ab9473d8e43b533836ab456299c88712103372b34234ed7cf9c1fea5d05d441557927be9542b162eb02e1ab2ce80224c00b52ae"))))
    assert(spendingInfo.conditionalPath == ConditionalPath.NoConditionsLeft)
  }

  it must "fail to create a valid UTXOSpendingInfo from a PSBTInputMap with insufficient data" in {
    val psbt1 = PSBT(
      "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a010000000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0000")
    assertThrows[UnsupportedOperationException](
      psbt1.getUTXOSpendingInfoUsingSigners(index = 0,
                                            getDummySigners(size = 1)))
  }

  it must "correctly construct and sign a PSBT" in {
    forAllAsync(PSBTGenerators.psbtToBeSigned) { psbtWithBuilderF =>
      psbtWithBuilderF.flatMap {
        case (psbtNoSigs, utxos) =>
          val infos = utxos.toVector.zipWithIndex.map {
            case (utxo: BitcoinUTXOSpendingInfoFull, index) =>
              (index, utxo)
          }
          val signedPSBTF = infos.foldLeft(Future.successful(psbtNoSigs)) {
            case (unsignedPSBTF, (index, info)) =>
              unsignedPSBTF.flatMap { unsignedPSBT =>
                info.toSingles.foldLeft(Future.successful(unsignedPSBT)) {
                  (psbtToSignF, singleInfo) =>
                    psbtToSignF.flatMap(
                      _.sign(index,
                             singleInfo.signer,
                             singleInfo.conditionalPath))
                }
              }
          }
          signedPSBTF.map { signedPSBT =>
            val finalizedPsbtT = signedPSBT.finalizePSBT
            finalizedPsbtT match {
              case Success(finalizedPsbt) =>
                assert(finalizedPsbt.extractTransactionAndValidate.isSuccess)
              case Failure(exception) => fail(exception)
            }
          }
      }
    }
  }

  it must "add Redeem Scripts to outputs" in {
    forAllAsync(PSBTGenerators.psbtWithBuilderAndP2SHOutputs(finalized = false)) {
      psbtWithBuilderF =>
        psbtWithBuilderF.flatMap {
          case (psbtEmptyOutputs, _, redeemScripts) =>
            val psbtWithOutputs = redeemScripts.zipWithIndex.foldLeft(
              psbtEmptyOutputs)((psbt, spk) =>
              psbt.addRedeemOrWitnessScriptToOutput(spk._1, spk._2))

            val allOutputsValid =
              psbtWithOutputs.outputMaps.zip(redeemScripts).forall {
                case (map, spk) =>
                  map.redeemScriptOpt.contains(RedeemScript(spk))
              }
            assert(allOutputsValid)
        }
    }
  }

  it must "add Witness Scripts to outputs" in {
    forAllAsync(
      PSBTGenerators.psbtWithBuilderAndP2WSHOutputs(finalized = false)) {
      psbtWithBuilderF =>
        psbtWithBuilderF.flatMap {
          case (psbtEmptyOutputs, _, redeemScripts) =>
            val psbtWithOutputs = redeemScripts.zipWithIndex.foldLeft(
              psbtEmptyOutputs)((psbt, spk) =>
              psbt.addRedeemOrWitnessScriptToOutput(spk._1, spk._2))

            val allOutputsValid =
              psbtWithOutputs.outputMaps.zip(redeemScripts).forall {
                case (map, spk) =>
                  map.witnessScriptOpt.contains(WitnessScript(spk))

              }
            assert(allOutputsValid)
        }
    }
  }

  it must "correctly construct and finalize PSBTs from UTXOSpendingInfo" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs(),
                ScriptGenerators.scriptPubKey,
                ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfo, destinations), (changeSPK, _), network) =>
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        val maxFee = crediting - spending
        val fee = GenUtil.sample(CurrencyUnitGenerator.feeUnit(maxFee))
        for {
          (psbt, _) <- PSBTGenerators.psbtAndBuilderFromInputs(
            finalized = false,
            creditingTxsInfo = creditingTxsInfo,
            destinations = destinations,
            changeSPK = changeSPK,
            network = network,
            fee = fee)
          (expected, _) <- PSBTGenerators.psbtAndBuilderFromInputs(
            finalized = true,
            creditingTxsInfo = creditingTxsInfo,
            destinations = destinations,
            changeSPK = changeSPK,
            network = network,
            fee = fee)
        } yield {
          val finalizedPsbtOpt = psbt.finalizePSBT
          assert(finalizedPsbtOpt.isSuccess, psbt.hex)
          assert(finalizedPsbtOpt.get == expected)
        }
    }
  }

  it must "agree with TxBuilder.sign given UTXOSpendingInfos" in {
    forAllAsync(PSBTGenerators.finalizedPSBTWithBuilder) { psbtAndBuilderF =>
      for {
        (psbt, builder) <- psbtAndBuilderF
        signedTx <- builder.sign
      } yield {
        val txT = psbt.extractTransactionAndValidate
        assert(txT.isSuccess, txT.failed)

        assert(txT.get == signedTx)
      }
    }
  }

  it must "fail to create an Unknown PSBTRecord from with a known KeyId" in {
    // Key 0x00 is used for all types of records
    val key = hex"00"
    val value =
      hex"000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f"
    assertThrows[IllegalArgumentException](GlobalPSBTRecord.Unknown(key, value))
    assertThrows[IllegalArgumentException](InputPSBTRecord.Unknown(key, value))
    assertThrows[IllegalArgumentException](OutputPSBTRecord.Unknown(key, value))
  }

  // Test case given in https://github.com/bitcoin/bips/blob/master/bip-0174.mediawiki#test-vectors
  it must "sign a PSBT" in {
    val unsignedPsbt = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f618765000000010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8870103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val privKey0 = ECPrivateKey.fromWIFToPrivateKey(
      "cP53pDbR5WtAD8dYAW9hhTjuvvTVaEiQBdrz9XPrgLBeRFiyCbQr")
    val privKey1 = ECPrivateKey.fromWIFToPrivateKey(
      "cR6SXDoyfQrcp4piaiHE97Rsgta9mNhGTen9XeonVgwsh4iSgw6d")

    val expectedPsbt0 = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000002202029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e887220203089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f010103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val expectedPsbt1 = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f618765000000220202dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d7483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8872202023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e73473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d2010103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val privKey2 = ECPrivateKey.fromWIFToPrivateKey(
      "cT7J9YpCwY3AVRFSjN6ukeEeWY6mhpbJPxRaDaP5QTdygQRxP9Au")

    val privKey3 = ECPrivateKey.fromWIFToPrivateKey(
      "cNBc3SWUip9PPm1GjRoLEJT6T41iNzCYtD7qro84FMnM5zEqeJsE")

    for {
      firstSig0 <- unsignedPsbt.sign(inputIndex = 0, signer = privKey0)
      signedPsbt0 <- firstSig0.sign(inputIndex = 1, signer = privKey1)

      firstSig1 <- unsignedPsbt.sign(inputIndex = 0, signer = privKey2)
      signedPsbt1 <- firstSig1.sign(inputIndex = 1, signer = privKey3)
    } yield {
      assert(signedPsbt0 == expectedPsbt0)
      assert(signedPsbt1 == expectedPsbt1)
    }
  }

  def dummyTx(scriptSig: ScriptSignature = EmptyScriptSignature,
              spk: ScriptPubKey = EmptyScriptPubKey): Transaction = {
    BaseTransaction(
      version = Int32.zero,
      inputs = Vector(
        TransactionInput(outPoint =
                           TransactionOutPoint(txId = DoubleSha256Digest.empty,
                                               vout = UInt32.zero),
                         scriptSignature = scriptSig,
                         sequenceNumber = UInt32.zero)),
      outputs = Vector(TransactionOutput(CurrencyUnits.oneBTC, spk)),
      lockTime = UInt32.zero
    )
  }

  def dummyPSBT(scriptSig: ScriptSignature = EmptyScriptSignature,
                spk: ScriptPubKey = EmptyScriptPubKey): PSBT = {
    PSBT.fromUnsignedTx(dummyTx(scriptSig, spk))
  }

  it must "successfully change a NonWitnessUTXO to a WitnessUTXO when compressing" in {
    val wspk = P2WSHWitnessSPKV0(EmptyScriptPubKey)

    val output =
      TransactionOutput(value = CurrencyUnits.oneBTC, scriptPubKey = wspk)

    val tx = dummyTx(spk = wspk)
    val inputMap =
      InputPSBTMap(Vector(InputPSBTRecord.NonWitnessOrUnknownUTXO(tx)))

    val expectedInputMap =
      InputPSBTMap(Vector(InputPSBTRecord.WitnessUTXO(output)))

    val input = TransactionInput(
      outPoint = TransactionOutPoint(txId = tx.txId, vout = UInt32.zero),
      scriptSignature = EmptyScriptSignature,
      sequenceNumber = UInt32.zero)

    val compressedInputMap = inputMap.compressMap(input)

    assert(compressedInputMap == expectedInputMap)
  }

  it must "do nothing when compressing a finalized InputPSBTMap" in {
    val finalizedInputMap = InputPSBTMap(
      Vector(InputPSBTRecord.FinalizedScriptSig(EmptyScriptSignature)))

    val compressedInputMap =
      finalizedInputMap.compressMap(EmptyTransactionInput)

    assert(compressedInputMap == finalizedInputMap)
  }

  it must "be able to filter OutputPSBTMap records" in {
    val redeemScriptRecord = OutputPSBTRecord.RedeemScript(EmptyScriptPubKey)
    val outputMap = OutputPSBTMap(
      Vector(redeemScriptRecord,
             OutputPSBTRecord.BIP32DerivationPath(ECPublicKey.freshPublicKey,
                                                  ExtKey.masterFingerprint,
                                                  BIP32Path.empty)))

    val expectedElements = Vector(redeemScriptRecord)

    val filteredElements =
      outputMap.filterRecords(PSBTOutputKeyId.BIP32DerivationPathKeyId)

    assert(filteredElements == expectedElements)
  }

  it must "fail to finalize an already finalized PSBT" in {
    val psbt = dummyPSBT()

    val finalizedInputMap = InputPSBTMap(
      Vector(InputPSBTRecord.FinalizedScriptSig(EmptyScriptSignature)))
    val finalizedPSBT =
      PSBT(psbt.globalMap, Vector(finalizedInputMap), psbt.outputMaps)

    assert(finalizedPSBT.isFinalized)

    val finalizeTry = finalizedPSBT.finalizePSBT
    assert(finalizeTry.isFailure)
    assert(finalizeTry.failed.get.isInstanceOf[IllegalStateException])
  }

  it must "fail to add ScriptWitness to input when script hashes are wrong" in {
    val wspk = P2WSHWitnessSPKV0(P2PKScriptPubKey(ECPublicKey.freshPublicKey))
    val p2sh = P2SHScriptPubKey(wspk)
    val badRedeemScript = P2PKScriptPubKey(ECPublicKey.freshPublicKey)

    val psbtP2WSH = dummyPSBT(spk = wspk)
    val psbtP2SH = dummyPSBT(spk = p2sh)

    assertThrows[IllegalArgumentException](
      psbtP2WSH.addRedeemOrWitnessScriptToOutput(script = badRedeemScript,
                                                 index = 0))
    assertThrows[IllegalArgumentException](
      psbtP2SH.addRedeemOrWitnessScriptToOutput(script = badRedeemScript,
                                                index = 0))
  }

  it must "fail to add an EmptyScriptWitness to an input" in {
    val psbt = dummyPSBT()

    assertThrows[IllegalArgumentException](
      psbt.addScriptWitnessToInput(EmptyScriptWitness, index = 0))
  }

  it must "fail to addRedeemOrWitnessScriptToOuput when finalized" in {
    val psbt = dummyPSBT()
    val finalizedInputMap = InputPSBTMap(
      Vector(InputPSBTRecord.FinalizedScriptSig(EmptyScriptSignature)))
    val finalizedPsbt =
      PSBT(psbt.globalMap, Vector(finalizedInputMap), psbt.outputMaps)

    assertThrows[IllegalArgumentException](
      finalizedPsbt.addRedeemOrWitnessScriptToOutput(EmptyScriptPubKey,
                                                     index = 0))
  }

  it must "addScriptWitnessToOutput correctly" in {
    val psbt = dummyPSBT()
    val p2wshWitness = P2WSHWitnessV0(EmptyScriptPubKey)

    assertThrows[IllegalArgumentException](
      psbt.addScriptWitnessToOutput(p2wshWitness, index = -1))
    assertThrows[IllegalArgumentException](
      psbt.addScriptWitnessToOutput(p2wshWitness, index = 1))

    val p2wpkhPSBT =
      psbt.addScriptWitnessToOutput(P2WPKHWitnessV0(ECPublicKey.freshPublicKey),
                                    index = 0)
    assert(p2wpkhPSBT == psbt)

    val p2wshPSBT = psbt.addScriptWitnessToOutput(p2wshWitness, index = 0)
    val expectedOutputMap =
      OutputPSBTMap(Vector(OutputPSBTRecord.WitnessScript(EmptyScriptPubKey)))
    val expectedPSBT =
      PSBT(psbt.globalMap, psbt.inputMaps, Vector(expectedOutputMap))
    assert(p2wshPSBT == expectedPSBT)
    assertThrows[IllegalArgumentException](
      p2wshPSBT.addScriptWitnessToOutput(p2wshWitness, index = 0))

    assertThrows[IllegalArgumentException](
      psbt.addScriptWitnessToOutput(EmptyScriptWitness, index = 0))

    val finalizedInputMap = InputPSBTMap(
      Vector(InputPSBTRecord.FinalizedScriptSig(EmptyScriptSignature)))
    val finalizedPsbt =
      PSBT(psbt.globalMap, Vector(finalizedInputMap), psbt.outputMaps)
    assertThrows[IllegalArgumentException](
      finalizedPsbt.addScriptWitnessToOutput(p2wshWitness, index = 0))
  }
}
