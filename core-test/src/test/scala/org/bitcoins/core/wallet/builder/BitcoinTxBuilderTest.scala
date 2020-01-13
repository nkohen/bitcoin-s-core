package org.bitcoins.core.wallet.builder

import org.bitcoins.core.config.TestNet3
import org.bitcoins.core.crypto.{
  BaseTxSigComponent,
  DoubleSha256DigestBE,
  ECPrivateKey,
  WitnessTxSigComponentP2SH,
  WitnessTxSigComponentRaw
}
import org.bitcoins.core.currency._
import org.bitcoins.testkit.core.gen.{
  ChainParamsGenerator,
  CreditingTxGen,
  ScriptGenerators
}
import org.bitcoins.core.byteVectorOrdering
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  ConditionalPath,
  LockTimeSpendingInfoFull,
  UTXOSpendingInfo
}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.psbt.PSBTInputKeyId.PartialSignatureKeyId
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder.UTXOMap
import org.bitcoins.core.wallet.signer.{BitcoinSigner, BitcoinSignerSingle}
import org.bitcoins.testkit.Implicits._
import org.bitcoins.testkit.util.BitcoinSAsyncTest

import scala.concurrent.Future

class BitcoinTxBuilderTest extends BitcoinSAsyncTest {
  val tc = TransactionConstants
  val (spk, privKey) = ScriptGenerators.p2pkhScriptPubKey.sampleSome

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "BitcoinTxBuilder"

  // We had a matcherror when passing in a vector of UTXOs,
  // because a match statement on a Seq relied on the ::
  // deconstructor. You would assume the compiler could
  // warn you about that...
  it must "work with a list and a vector of UTXOs" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Seq(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )

    val listF =
      BitcoinTxBuilder(destinations = Seq.empty,
                       utxos = List(utxo),
                       feeRate = SatoshisPerByte(1.sat),
                       changeSPK = EmptyScriptPubKey,
                       network = RegTest)

    val vecF =
      BitcoinTxBuilder(destinations = Seq.empty,
                       utxos = List(utxo),
                       feeRate = SatoshisPerByte(1.sat),
                       changeSPK = EmptyScriptPubKey,
                       network = RegTest)

    for {
      _ <- listF
      _ <- vecF
    } yield succeed

  }

  it must "failed to build a transaction that mints money out of thin air" in {

    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Seq(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    val resultFuture = txBuilder.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to build a transaction when we pass in a negative fee rate" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Seq(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(Satoshis(-1))
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    recoverToSucceededIf[IllegalArgumentException] {
      txBuilder
    }
  }

  it must "fail a transaction when the user invariants fail" in {
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, spk)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(outPoint,
                                       creditingOutput,
                                       Seq(privKey),
                                       None,
                                       None,
                                       HashType.sigHashAll,
                                       conditionalPath =
                                         ConditionalPath.NoConditionsLeft)
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val feeUnit = SatoshisPerVirtualByte(currencyUnit = Satoshis(1))
    val txBuilder = BitcoinTxBuilder(destinations = destinations,
                                     utxos = utxoMap,
                                     feeRate = feeUnit,
                                     changeSPK = EmptyScriptPubKey,
                                     network = TestNet3)
    //trivially false
    val f = (_: Seq[BitcoinUTXOSpendingInfo], _: Transaction) => false
    val resultFuture = txBuilder.flatMap(_.sign(f))
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "be able to create a BitcoinTxBuilder from UTXOTuple and UTXOMap" in {
    val creditingOutput =
      TransactionOutput(value = CurrencyUnits.zero, scriptPubKey = spk)
    val destinations = {
      Seq(
        TransactionOutput(value = Satoshis.one,
                          scriptPubKey = EmptyScriptPubKey))
    }
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Seq(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)
    val utxoSpendingInfo = BitcoinUTXOSpendingInfo(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Seq(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = None,
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderMap = BitcoinTxBuilder(destinations = destinations,
                                        utxos = utxoMap,
                                        feeRate = feeUnit,
                                        changeSPK = EmptyScriptPubKey,
                                        network = TestNet3)
    val txBuilderTuple = BitcoinTxBuilder(destinations = destinations,
                                          utxos = Seq(utxoSpendingInfo),
                                          feeRate = feeUnit,
                                          changeSPK = EmptyScriptPubKey,
                                          network = TestNet3)

    txBuilderTuple.flatMap { tup =>
      txBuilderMap.map { map =>
        assert(map == tup)
      }
    }
  }

  it must "fail to build a tx if you have the wrong redeemscript" in {
    val p2sh = P2SHScriptPubKey(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2sh)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = creditingOutput,
        signers = Seq(privKey),
        redeemScriptOpt = Some(EmptyScriptPubKey),
        scriptWitnessOpt = None,
        hashType = HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail to build a tx if you have the wrong script witness" in {
    val p2wsh = P2WSHWitnessSPKV0(spk)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wsh)
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint,
        creditingOutput,
        Seq(privKey),
        None,
        Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail to sign a p2pkh if we don't pass in the public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(
      outPoint,
      creditingOutput,
      Seq(privKey),
      None,
      Some(P2WSHWitnessV0(EmptyScriptPubKey)),
      HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderWitness = BitcoinTxBuilder(destinations,
                                            utxoMap,
                                            feeUnit,
                                            EmptyScriptPubKey,
                                            TestNet3)
    val resultFuture = txBuilderWitness.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to sign a p2pkh if we pass in the wrong public key" in {
    val p2pkh = P2PKHScriptPubKey(privKey.publicKey)
    val pubKey2 = ECPrivateKey().publicKey
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2pkh)
    val destinations =
      Seq(TransactionOutput(Satoshis.one, EmptyScriptPubKey))
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    val utxo = BitcoinUTXOSpendingInfo(
      outPoint = outPoint,
      output = creditingOutput,
      signers = Seq(privKey),
      redeemScriptOpt = None,
      scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
      hashType = HashType.sigHashAll,
      conditionalPath = ConditionalPath.NoConditionsLeft
    )
    val utxoMap: UTXOMap = Map(outPoint -> utxo)

    val feeUnit = SatoshisPerVirtualByte(Satoshis.one)
    val txBuilderWitness = BitcoinTxBuilder(destinations = destinations,
                                            utxos = utxoMap,
                                            feeRate = feeUnit,
                                            changeSPK = EmptyScriptPubKey,
                                            network = TestNet3)
    val resultFuture = txBuilderWitness.flatMap(_.sign)
    recoverToSucceededIf[IllegalArgumentException] {
      resultFuture
    }
  }

  it must "fail to sign a p2wpkh if we don't pass in the public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(pubKey = privKey.publicKey)
    val creditingOutput =
      TransactionOutput(value = CurrencyUnits.zero, scriptPubKey = p2wpkh)
    val creditingTx = BaseTransaction(version = tc.validLockVersion,
                                      inputs = Nil,
                                      outputs = Seq(creditingOutput),
                                      lockTime = tc.lockTime)
    val outPoint =
      TransactionOutPoint(txId = creditingTx.txId, vout = UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint = outPoint,
        output = creditingOutput,
        signers = Seq(privKey),
        redeemScriptOpt = None,
        scriptWitnessOpt = Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        hashType = HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "fail to sign a p2wpkh if we pass in the wrong public key" in {
    val p2wpkh = P2WPKHWitnessSPKV0(privKey.publicKey)
    val creditingOutput = TransactionOutput(CurrencyUnits.zero, p2wpkh)
    val creditingTx = BaseTransaction(tc.validLockVersion,
                                      Nil,
                                      Seq(creditingOutput),
                                      tc.lockTime)
    val outPoint = TransactionOutPoint(creditingTx.txId, UInt32.zero)
    assertThrows[IllegalArgumentException] {
      BitcoinUTXOSpendingInfo(
        outPoint,
        creditingOutput,
        Seq(privKey),
        None,
        Some(P2WSHWitnessV0(EmptyScriptPubKey)),
        HashType.sigHashAll,
        conditionalPath = ConditionalPath.NoConditionsLeft
      )
    }
  }

  it must "succeed to sign a cltv spk that uses a second-based locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = System.currentTimeMillis / 1000

    val cltvSPK =
      CLTVScriptPubKey(ScriptNumber(lockTime),
                       P2PKScriptPubKey(fundingPrivKey.publicKey))

    val cltvSpendingInfo = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      Bitcoins.one,
      cltvSPK,
      Vector(fundingPrivKey),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val txBuilderF =
      BitcoinTxBuilder(
        Vector(
          TransactionOutput(Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        Vector(cltvSpendingInfo),
        SatoshisPerByte(Satoshis.one),
        EmptyScriptPubKey,
        RegTest
      )

    txBuilderF
      .flatMap(_.sign)
      .map(tx => assert(tx.lockTime == UInt32(lockTime)))
  }

  it must "succeed to sign a cltv spk that uses a block height locktime" in {
    val fundingPrivKey = ECPrivateKey.freshPrivateKey

    val lockTime = 1000

    val cltvSPK =
      CLTVScriptPubKey(ScriptNumber(lockTime),
                       P2PKScriptPubKey(fundingPrivKey.publicKey))

    val cltvSpendingInfo = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      Bitcoins.one,
      cltvSPK,
      Vector(fundingPrivKey),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val txBuilderF =
      BitcoinTxBuilder(
        Vector(
          TransactionOutput(Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        Vector(cltvSpendingInfo),
        SatoshisPerByte(Satoshis.one),
        EmptyScriptPubKey,
        RegTest
      )

    txBuilderF
      .flatMap(_.sign)
      .map(tx => assert(tx.lockTime == UInt32(lockTime)))
  }

  it must "fail to sign a cltv spk that uses both a second-based and a block height locktime" in {
    val fundingPrivKey1 = ECPrivateKey.freshPrivateKey
    val fundingPrivKey2 = ECPrivateKey.freshPrivateKey

    val lockTime1 = System.currentTimeMillis / 1000
    val lockTime2 = 1000

    val cltvSPK1 =
      CLTVScriptPubKey(ScriptNumber(lockTime1),
                       P2PKScriptPubKey(fundingPrivKey1.publicKey))
    val cltvSPK2 =
      CLTVScriptPubKey(ScriptNumber(lockTime2),
                       P2PKScriptPubKey(fundingPrivKey2.publicKey))

    val cltvSpendingInfo1 = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      Bitcoins.one,
      cltvSPK1,
      Vector(fundingPrivKey1),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val cltvSpendingInfo2 = LockTimeSpendingInfoFull(
      TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
      Bitcoins.one,
      cltvSPK2,
      Vector(fundingPrivKey2),
      HashType.sigHashAll,
      ConditionalPath.NoConditionsLeft
    )

    val txBuilderF =
      BitcoinTxBuilder(
        Vector(
          TransactionOutput(Bitcoins.one + Bitcoins.one - CurrencyUnits.oneMBTC,
                            EmptyScriptPubKey)),
        Vector(cltvSpendingInfo1, cltvSpendingInfo2),
        SatoshisPerByte(Satoshis.one),
        EmptyScriptPubKey,
        RegTest
      )

    recoverToSucceededIf[IllegalArgumentException](
      txBuilderF.flatMap(_.unsignedTx)
    )
  }

  def verifyScript(tx: Transaction, utxos: Seq[UTXOSpendingInfo]): Boolean = {
    val programs: Seq[PreExecutionScriptProgram] = tx.inputs.zipWithIndex.map {
      case (input: TransactionInput, idx: Int) =>
        val outpoint = input.previousOutput

        val creditingTx = utxos.find(u => u.outPoint.txId == outpoint.txId).get

        val output = creditingTx.output

        val spk = output.scriptPubKey

        val amount = output.value

        val txSigComponent = spk match {
          case witSPK: WitnessScriptPubKeyV0 =>
            val o = TransactionOutput(amount, witSPK)
            WitnessTxSigComponentRaw(tx.asInstanceOf[WitnessTransaction],
                                     UInt32(idx),
                                     o,
                                     Policy.standardFlags)
          case _: UnassignedWitnessScriptPubKey => ???
          case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
              _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
              _: WitnessCommitment | _: CSVScriptPubKey | _: CLTVScriptPubKey |
              _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
              EmptyScriptPubKey) =>
            val o = TransactionOutput(CurrencyUnits.zero, x)
            BaseTxSigComponent(tx, UInt32(idx), o, Policy.standardFlags)

          case p2sh: P2SHScriptPubKey =>
            val p2shScriptSig =
              tx.inputs(idx).scriptSignature.asInstanceOf[P2SHScriptSignature]
            p2shScriptSig.redeemScript match {

              case _: WitnessScriptPubKey =>
                WitnessTxSigComponentP2SH(transaction =
                                            tx.asInstanceOf[WitnessTransaction],
                                          inputIndex = UInt32(idx),
                                          output = output,
                                          flags = Policy.standardFlags)

              case _ =>
                BaseTxSigComponent(tx,
                                   UInt32(idx),
                                   output,
                                   Policy.standardFlags)
            }
        }

        PreExecutionScriptProgram(txSigComponent)
    }
    ScriptInterpreter.runAllVerify(programs)
  }

  it must "sign a mix of spks in a tx and then have it verified" in {
    forAllAsync(CreditingTxGen.inputsAndOuptuts,
                ScriptGenerators.scriptPubKey,
                ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfo, destinations), changeSPK, network) =>
        val fee = SatoshisPerVirtualByte(Satoshis(1000))
        val builder = BitcoinTxBuilder(destinations,
                                       creditingTxsInfo,
                                       fee,
                                       changeSPK._1,
                                       network)
        val txF = builder.flatMap(_.sign)

        txF.map { tx =>
          assert(verifyScript(tx, creditingTxsInfo))
        }
    }
  }

  it must "sign a mix of p2sh/p2wsh in a tx and then have it verified" in {
    forAllAsync(CreditingTxGen.inputsAndOuptuts,
                ScriptGenerators.scriptPubKey,
                ChainParamsGenerator.bitcoinNetworkParams) {
      case ((creditingTxsInfo, destinations), changeSPK, network) =>
        val fee = SatoshisPerByte(Satoshis(1000))
        val builder = BitcoinTxBuilder(destinations,
                                       creditingTxsInfo,
                                       fee,
                                       changeSPK._1,
                                       network)
        val txF = builder.flatMap(_.sign)

        txF.map { tx =>
          assert(verifyScript(tx, creditingTxsInfo))
        }
    }
  }

  it should "sign a PSBT" in {
    val unsignedPsbt = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f618765000000010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8870103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val privKey0 = ECPrivateKey.fromWIFToPrivateKey(
      "cP53pDbR5WtAD8dYAW9hhTjuvvTVaEiQBdrz9XPrgLBeRFiyCbQr")
    val privKey1 = ECPrivateKey.fromWIFToPrivateKey(
      "cR6SXDoyfQrcp4piaiHE97Rsgta9mNhGTen9XeonVgwsh4iSgw6d")

    val expectedPsbt = PSBT(
      "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000002202029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f0000008000000080010000800001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e887220203089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f010103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

    val signedPsbt: Future[PSBT] = BitcoinSignerSingle
      .sign(unsignedPsbt, 0, privKey0, isDummySignature = false)
      .flatMap(x =>
        BitcoinSignerSingle
          .sign(x, 1, privKey1, isDummySignature = false))

    signedPsbt.map { signedPsbt =>
      expectedPsbt.inputMaps.head.elements
        .sortBy(_.key)
        .foreach(x => println(x))

      println()
      println()
      println()

      signedPsbt.inputMaps.head.elements
        .sortBy(_.key)
        .foreach(x => println(x))

      println()
      println()
      println()

      expectedPsbt.inputMaps.last.elements
        .sortBy(_.key)
        .foreach(x => println(x))

      println()
      println()
      println()

      signedPsbt.inputMaps.last.elements
        .sortBy(_.key)
        .foreach(x => println(x))

      assert(
        signedPsbt.inputMaps.head.bytes == expectedPsbt.inputMaps.head.bytes)
      assert(
        signedPsbt.inputMaps.last.bytes == expectedPsbt.inputMaps.last.bytes)

      assert(signedPsbt.bytes == expectedPsbt.bytes)
    }
  }
}
