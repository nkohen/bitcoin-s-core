package org.bitcoins.crypto

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers
import scodec.bits.ByteVector

class SipHashTest extends AnyFlatSpec with Matchers {

  val sipHashData = Vector(
    "data,key,siphash",
    "02f10b8cd5db552585f6153788d4177d04ca1491c53ffc94f1d61babcfd60431,360b5226337ec89d487fe3865abc6d07,-8128209201632884650",
    "8ce2353ab1a8e107ddc0b5c74ee95ea444081598dc50789119a556a18a27500a,f0d3b68c4cfeace177e7550c154ea36f,-4406396627635197047",
    "ebade2875677fe4d2368ee95c96e4eb4e0808b6fce19a6280311f959a91869c6,9eff67466d03f8f12e62a339a5de0f78,6769578077810524633",
    "ac1e296812e4b933166c49f3495d79cbfeba2e10ef6f1c0efdbe8a829af1aa0a,b68134a9459056625f21da3f8da9f853,-2583001939764064527",
    "3c76bd64e35aeba7f65dfe8788f8da088b54b4b5f83cd9c9ce04bd1d219ab0eb,395b097b37143d5bc7020a0c03493909,8736731183777772005",
    "0b40f96925d31e817e9353c8d0a65b4357e4ffb5855fb9488cac2462392392e3,4d8fdee0a03f2fa78a6ab02d5f79b8ae,-573818258625669565",
    "43c53413b1d6c02a57a01aa19c28fca726efdbd338de98208fbf706d57c4619d,f0cf15e99ef087d49dcc21937017952f,-7397194943924165122",
    "46b5fc93bfc5aeb3118b1dc16db0c3a77b15a2b4ca153a8a8c84db9ab647963a,e15c0bfd8a379c3fd3cc2abb6ae7f0f0,923345511469621509",
    "3669e5ed84755ddd6a1ab1c51bb602670a700eb898413a84fa2b06ebb30dd21c,7fce57625f6e6bab0ea5cdef154d0377,-1832758371336483690",
    "d5cae677498a0333b02c64a55ce9292d5349f7ffcf1cdd1e36aff6d08d9aced6,a4d7adda21fd1cdd7d990d29680611a8,7149213847731948880",
    "512c79095863c4acac9bd36311969b16f5376d41d470d2590cb734918b5968c9,04e3808bd7dc4298e256a0fea5c4cb5f,3288384153810440668",
    "793ceaaa99e4c8b8d96156bce8aacb39494b6fd7f95a813411af7a359bf967f3,9443d9852369d65fe9744c9c2a1e4715,4543948005932978032",
    "0a4ec5492af807a88118a62b2147da115fff279b9b86b5b0625af39a27166423,b523560ab9b9ef546aba5e75915fa30e,-7390404382480069649",
    "c51fed381d14dee038dbc6e6bab3a61da08ac1db5da950454383f50d029ff925,74eaf0c59859ee299d26cd183ef5e1d0,2377911775285818461",
    "fd0cce52675fc7593fd9e36f04357f6d44cf7c1ebb781f0d460b2b82bc046c0d,c6ade316f492713ddb59e8d02ee20b96,2384081868434014232",
    "8f12b7c16e2c9fc61a2cd869f95fc65da404216ac4542217d33c5135561d5c17,e4638504c41785fa60eb151bd1d7b386,-2965963461982941101"
  ).tail.map { line =>
    val arr = line.split(",")
    (ByteVector.fromValidHex(arr(0)),
     SipHashKey(ByteVector.fromValidHex(arr(1))),
     arr(2).toLong)
  }

  it must "compute SipHash" in {
    sipHashData.foreach { case (data, key, expected) =>
      assert(CryptoUtil.sipHash(data, key) == expected)
    }
  }

}