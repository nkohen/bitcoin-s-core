package org.bitcoins.crypto

import org.scalatest.Assertion
import scodec.bits.ByteVector

/** Executes tests at https://github.com/discreetlogcontracts/dlcspecs/blob/9b11bcc99341ad40d8ae146d5d3ead116d5bb131/test/ecdsa_adaptor.json
  * along with some static signing tests Jesse generated.
  */
class BouncyCastleECAdaptorSignatureTest extends BitcoinSCryptoTest {
  behavior of "Bouncy Castle ECDSA Adaptor Signing"

  def testSign(
      secKey: String,
      msg: String,
      decKey: String,
      pubKey: String,
      encKey: String,
      auxRand: String,
      adaptorSig: String): Assertion = {
    testSign(
      ECPrivateKey(secKey),
      ByteVector.fromValidHex(msg),
      ECPrivateKey(decKey),
      ECPublicKey(pubKey),
      ECPublicKey(encKey),
      ByteVector.fromValidHex(auxRand),
      ECAdaptorSignature(adaptorSig)
    )
  }

  def testSign(
      secKey: ECPrivateKey,
      msg: ByteVector,
      decKey: ECPrivateKey,
      pubKey: ECPublicKey,
      encKey: ECPublicKey,
      auxRand: ByteVector,
      adaptorSig: ECAdaptorSignature): Assertion = {
    assert(pubKey == secKey.publicKey)
    assert(
      BouncycastleCryptoRuntime
        .adaptorSign(secKey, encKey, msg, auxRand) == adaptorSig)
    assert(
      BouncycastleCryptoRuntime.adaptorVerify(adaptorSig, pubKey, msg, encKey))

    val sigBC = BouncycastleCryptoRuntime.adaptorComplete(decKey, adaptorSig)
    assert(
      BouncycastleCryptoRuntime
        .extractAdaptorSecret(sigBC, adaptorSig, encKey) == decKey)
  }

  it must "pass Jesse's static test vectors" in {
    testSign(
      secKey =
        "00033065FBFD7BABE601089E75C463D16E8FB8C731ED520DE425585099E27F70",
      msg = "9BB3A7C4CF1C10C225FF6C78867352559C6B0AD43DA0744BF0744A1C520FB03A",
      decKey =
        "918B8E4F61F44EA1EF3F3C236973F2F12796E0A028FCEC3F30D71BF1C035A95A",
      pubKey =
        "036ED79915CEAAFBFDACF7D3ABF105991B28922A90B8E7BEAE69E5413A599AE6BF",
      encKey =
        "038B3E8F09A59F17A3CC7B8BDF04038CD102CDBE6A9FFCFFF1A7A8EA749826F48D",
      auxRand =
        "580E40DCE3C772ED359FE6D9C1459702016845F1981ECC04E371B00E7B851ACA",
      adaptorSig =
        "0389CFE2E8861E763EA33C64C5034C292011A34CB8EAEC5376200B9D7D35F9304C03D951170788AEC19A94100ED8381040BD22C5A60F7264B53E51567D36A3DA9F5F30DC49EBC212AEED4535EA39C8B8F9418AFC8B4E8899C8C978B4440C4EC4474FD90760B4037045C2DA538AB237B1DCE99209A0093949A472F24F44C6A7F25084B47ECB9F342D5E5249BFFB3C9B6E3BCD5E3356558615CD0B256C53E13F74D753"
    )

    testSign(
      secKey =
        "B700B67DF0ADC70A715C6883CA2AB21976E33E974094F430E8932C3C64F88D49",
      msg = "3F690F3844B9ACF40D2CF91383BD6912CEB306FF909FC4DB7022372F8943A0F7",
      decKey =
        "BF984ABB08D8DAE02F0A3EC7626AD5EF95458732B7D1CA49D027F7A87CD754AC",
      pubKey =
        "026CA09B06BC3D11A5EF8EC4AF8BD7BFF8AEFE5257FA9C058698747D2E7C0443CB",
      encKey =
        "0258ECE4ABAFF624689160C197057E6BAF6D1E539114CF84B63E544EEFA970E006",
      auxRand =
        "2938CC748CEC98E96C2A1B45FC1F8B9036A0547ACFC6D81C0EFA439058ED6423",
      adaptorSig =
        "02A4A2900432C36D8ADA1E6124E22D9E902744814EEF3A0E82399B0F440EEBDE790262309993372A9602344771B0B23D6BC6F6CC168F72DEBA0B964134DE418A4C3A863D8EFCCE991BBD3B2AB990863D61F6C5D6826AE04F2A6D140A46E6CBA340CE527B7889B7FAE3253CD2E0C49D9A0DA23A08FB18B2A22C5238F81535674E20FD00FDAC5C53EA54D51DCF3290D231DD5FC164ED6756F85883668EBAAF4FA86CB1"
    )

    testSign(
      secKey =
        "29999EEF451EB2E62772A965329967F8B75CEB1E15B0FFB596B6E0C9CA7D2127",
      msg = "D9B5C45FFB11BC6425E7786014A2ADABCE6A75580CC5B43EFF30FE2261F98F6F",
      decKey =
        "16B30482C5BE8E19ACFB869749EAB4383DF44416E7B329720E00809E014E5088",
      pubKey =
        "02A3A0913F145690141DC2CBD33CCBBD204A8793B4F037718199D7625DA6A84B7B",
      encKey =
        "02A7E8E1E648D7977AE8A9AD1E7E77D875D098E34717E5B66183BF0969873B8B70",
      auxRand =
        "4A259D87BFFAE9E6A18B5CCD68E5FA24AEBCB424D22F015ACD27FE66FE007C90",
      adaptorSig =
        "03485D1947850BEA0DEEB384F978D0C423E1D9E19AF5B0BB23DB7524C4556B4838025815E5A01897A0C6731F91718B4933F342822BE8995EEEDD7C755746825E4D8B885C5F737D408D105D50B41AE1A107EBD82C9ADA567D4B71AEC808291CA8287A3087E5438B64C83806B36ABAFE69B4AF373209C02DF5516DFF83114188BD7457893D7691842AB373C049A17663505F11F23223119E5FBE2BCA9892E33C8726E6"
    )

    testSign(
      secKey =
        "BA837CC074A2CABAB78266230F01B8241937FC76358230736745E9CCB387D4FC",
      msg = "2E5BBCDBF24618F586910CB2DB7F4475EAD4F2C37DC4115C0DDC0ABB537917E5",
      decKey =
        "AF81094DB8EEA02499C1F98A385BCE55EB00683FC5A4141A50B344671DC76EE3",
      pubKey =
        "0384F22B3539B17727395C42BB0CCF81A6952FC7F3F561942EEA173B46B84AA890",
      encKey =
        "02D9D918CE57AEF470F4E5B543D7599330502CBEAB1285EDA545E38A37F2A32325",
      auxRand =
        "B32E2416E5259212F037C5E83688AF83EBF0AE73B757FEB26ACBD19199D63BCD",
      adaptorSig =
        "039E956529933038083DA428AA2E333AAFD35947A8D11997670DA0287987410ED302604BF918A20793981BB9E19B5B677EE764BA23E4FA09A7726D1FA37AE179F0160CFFFCB792F9A46D0240A424265454BB971CC9175763E8A9772635A61C253A39875826BBEB83779ED05144CE35E8B6B2A3469EB3A949543119AC28CC27AFD1D299520B2A7ED5CA1D814BA96EB80750A9F56F878F7D2D26E70B2245A39DE9AB59"
    )

    testSign(
      secKey =
        "AEEF60CF249A0CFE57B971A0624B41D6E7007F82B8FE86A568F2E9DC5CE01C6D",
      msg = "45131FDB8F3AED1AE4040BC43EB1D24AD144D9085B35C3F7036C490EFD492D9A",
      decKey =
        "AFD71563537C0E8438EE7FA047C91890471F5C699A63F2C0520C5CF99050FCB9",
      pubKey =
        "03789E4707B057FE03771827A344B1BF13B21307DA5E88726BEBDFFB601EA863AE",
      encKey =
        "0224F1566286D1D49189250F878710AF5A2F3658FC7633E4B33F825561B2E3BBAE",
      auxRand =
        "034FB76F56207BA7D8BD7C944C82A2198B0B61CE31160F6ED5B8CC642A1C55AA",
      adaptorSig =
        "02B90DDC325AF9A443B8845A10D3D3FB6A90D9937E4AF56E2E814106AEE976A9D2024FA51F36F15F4C4390FD881B01EB5C9488928F626C3104442BA230EE890B08FC656096218AF24A8B92ED518F9FE1F13213DB771187F95640ECAF02658A2ABA40A9CD5E89DB8215879A294CCE4E72786FE5E2267BC25F934CE34CFBF2FEF361A0BDFF1B2132552C0BD0C699A3195C5EA6D82C50B784175DE54E4924BCD15C06DB"
    )
  }

  it must "pass happy path verification test" in {
    val adaptorSig = ECAdaptorSignature(
      "03424d14a5471c048ab87b3b83f6085d125d5864249ae4297a57c84e74710bb6730223f325042fce535d040fee52ec13231bf709ccd84233c6944b90317e62528b2527dff9d659a96db4c99f9750168308633c1867b70f3a18fb0f4539a1aecedcd1fc0148fc22f36b6303083ece3f872b18e35d368b3958efe5fb081f7716736ccb598d269aa3084d57e1855e1ea9a45efc10463bbf32ae378029f5763ceb40173f")
    val msg = ByteVector.fromValidHex(
      "8131e6f4b45754f2c90bd06688ceeabc0c45055460729928b4eecf11026a9e2d")
    val pubKey = ECPublicKey(
      "035be5e9478209674a96e60f1f037f6176540fd001fa1d64694770c56a7709c42c")
    val encKey = ECPublicKey(
      "02c2662c97488b07b6e819124b8989849206334a4c2fbdf691f7b34d2b16e9c293")
    val decKey = ECPrivateKey(
      "0b2aba63b885a0f0e96fa0f303920c7fb7431ddfa94376ad94d969fbf4109dc8")
    val signature = ECDigitalSignature.fromRS(
      "424d14a5471c048ab87b3b83f6085d125d5864249ae4297a57c84e74710bb67329e80e0ee60e57af3e625bbae1672b1ecaa58effe613426b024fa1621d903394")

    assert(
      BouncycastleCryptoRuntime.adaptorVerify(adaptorSig, pubKey, msg, encKey))
    assert(
      BouncycastleCryptoRuntime.adaptorComplete(decKey,
                                                adaptorSig) == signature)
    assert(
      BouncycastleCryptoRuntime.extractAdaptorSecret(signature,
                                                     adaptorSig,
                                                     encKey) == decKey)
  }

  it must "pass ECDSA malleable verification test" in {
    val adaptorSig = ECAdaptorSignature(
      "036035c89860ec62ad153f69b5b3077bcd08fbb0d28dc7f7f6df4a05cca35455be037043b63c56f6317d9928e8f91007335748c49824220db14ad10d80a5d00a9654af0996c1824c64c90b951bb2734aaecf78d4b36131a47238c3fa2ba25e2ced54255b06df696de1483c3767242a3728826e05f79e3981e12553355bba8a0131cd370e63e3da73106f638576a5aab0ea6d45c042574c0c8d0b14b8c7c01cfe9072")
    val msg = ByteVector.fromValidHex(
      "8131e6f4b45754f2c90bd06688ceeabc0c45055460729928b4eecf11026a9e2d")
    val pubKey = ECPublicKey(
      "035be5e9478209674a96e60f1f037f6176540fd001fa1d64694770c56a7709c42c")
    val encKey = ECPublicKey(
      "024eee18be9a5a5224000f916c80b393447989e7194bc0b0f1ad7a03369702bb51")
    val decKey = ECPrivateKey(
      "db2debddb002473a001dd70b06f6c97bdcd1c46ba1001237fe0ee1aeffb2b6c4")
    val signature = ECDigitalSignature.fromRS(
      "6035c89860ec62ad153f69b5b3077bcd08fbb0d28dc7f7f6df4a05cca35455be4ceacf921546c03dd1be596723ad1e7691bdac73d88cc36c421c5e7f08384305")

    assert(
      BouncycastleCryptoRuntime.adaptorVerify(adaptorSig, pubKey, msg, encKey))
    assert(
      BouncycastleCryptoRuntime.adaptorComplete(decKey,
                                                adaptorSig) == signature)
    assert(
      BouncycastleCryptoRuntime.extractAdaptorSecret(signature,
                                                     adaptorSig,
                                                     encKey) == decKey)
  }

  it must "fail to validate a false proof" in {
    val adaptorSig = ECAdaptorSignature(
      "03f94dca206d7582c015fb9bffe4e43b14591b30ef7d2b464d103ec5e116595dba03127f8ac3533d249280332474339000922eb6a58e3b9bf4fc7e01e4b4df2b7a4100a1e089f16e5d70bb89f961516f1de0684cc79db978495df2f399b0d01ed7240fa6e3252aedb58bdc6b5877b0c602628a235dd1ccaebdddcbe96198c0c21bead7b05f423b673d14d206fa1507b2dbe2722af792b8c266fc25a2d901d7e2c335")
    val msg = ByteVector.fromValidHex(
      "8131e6f4b45754f2c90bd06688ceeabc0c45055460729928b4eecf11026a9e2d")
    val pubKey = ECPublicKey(
      "035be5e9478209674a96e60f1f037f6176540fd001fa1d64694770c56a7709c42c")
    val encKey = ECPublicKey(
      "0214ccb756249ad6e733c80285ea7ac2ee12ffebbcee4e556e6810793a60c45ad4")

    assert(
      !BouncycastleCryptoRuntime.adaptorVerify(adaptorSig, pubKey, msg, encKey))
  }

  it must "pass happy path recovery test" in {
    val adaptorSig = ECAdaptorSignature(
      "03f2db6e9ed33092cc0b898fd6b282e99bdaeccb3de85c2d2512d8d507f9abab290210c01b5bed7094a12664aeaab3402d8709a8f362b140328d1b36dd7cb420d02fb66b1230d61c16d0cd0a2a02246d5ac7848dcd6f04fe627053cd3c7015a7d4aa6ac2b04347348bd67da43be8722515d99a7985fbfa66f0365c701de76ff0400dffdc9fa84dddf413a729823b16af60aa6361bc32e7cfd6701e32957c72ace67b")
    val encKey = ECPublicKey(
      "027ee4f899bc9c5f2b626fa1a9b37ce291c0388b5227e90b0fd8f4fa576164ede7")
    val decKey = ECPrivateKey(
      "9cf3ea9be594366b78c457162908af3c2ea177058177e9c6bf99047927773a06")
    val signature = ECDigitalSignature.fromRS(
      "f2db6e9ed33092cc0b898fd6b282e99bdaeccb3de85c2d2512d8d507f9abab2921811fe7b53becf3b7affa9442abaa93c0ab8a8e45cd7ee2ea8d258bfc25d464")

    assert(
      BouncycastleCryptoRuntime.extractAdaptorSecret(signature,
                                                     adaptorSig,
                                                     encKey) == decKey)
  }

  it must "fail to extract a secret on unrelated signatures" in {
    val adaptorSig = ECAdaptorSignature(
      "03aa86d78059a91059c29ec1a757c4dc029ff636a1e6c1142fefe1e9d7339617c003a8153e50c0c8574a38d389e61bbb0b5815169e060924e4b5f2e78ff13aa7ad858e0c27c4b9eed9d60521b3f54ff83ca4774be5fb3a680f820a35e8840f4aaf2de88e7c5cff38a37b78725904ef97bb82341328d55987019bd38ae1745e3efe0f8ea8bdfede0d378fc1f96e944a7505249f41e93781509ee0bade77290d39cd12")
    val encKey = ECPublicKey(
      "035176d24129741b0fcaa5fd6750727ce30860447e0a92c9ebebdeb7c3f93995ed")
    val signature = ECDigitalSignature.fromRS(
      "f7f7fe6bd056fc4abd70d335f72d0aa1e8406bba68f3e579e4789475323564a452c46176c7fb40aa37d5651341f55697dab27d84a213b30c93011a7790bace8c")

    assertThrows[Exception](
      BouncycastleCryptoRuntime
        .extractAdaptorSecret(signature, adaptorSig, encKey))
  }

  it must "pass recovery test for high s value" in {
    val adaptorSig = ECAdaptorSignature(
      "032c637cd797dd8c2ce261907ed43e82d6d1a48cbabbbece801133dd8d70a01b1403eb615a3e59b1cbbf4f87acaf645be1eda32a066611f35dd5557802802b14b19c81c04c3fefac5783b2077bd43fa0a39ab8a64d4d78332a5d621ea23eca46bc011011ab82dda6deb85699f508744d70d4134bea03f784d285b5c6c15a56e4e1fab4bc356abbdebb3b8fe1e55e6dd6d2a9ea457e91b2e6642fae69f9dbb5258854")
    val encKey = ECPublicKey(
      "02042537e913ad74c4bbd8da9607ad3b9cb297d08e014afc51133083f1bd687a62")
    val decKey = ECPrivateKey(
      "324719b51ff2474c9438eb76494b0dc0bcceeb529f0a5428fd198ad8f886e99c")
    val signature = ECDigitalSignature.fromRS(
      "2c637cd797dd8c2ce261907ed43e82d6d1a48cbabbbece801133dd8d70a01b14b5f24321f550b7b9dd06ee4fcfd82bdad8b142ff93a790cc4d9f7962b38c6a3b")

    assert(
      BouncycastleCryptoRuntime.extractAdaptorSecret(signature,
                                                     adaptorSig,
                                                     encKey) == decKey)
  }
}
