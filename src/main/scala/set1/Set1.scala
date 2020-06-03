import cryptopals.helpers.{CryptoHelpers, DecodedResult, SingleCharXorResult}

import scala.io.BufferedSource

class Set1 extends CryptoHelpers {
  // Challenge 1
  def convertHexToBase64(hexString: String): String = {
    hexToBase64(hexString)
  }

  // Challenge 2
  def fixedXor(firstHex: String, secondHex: String): String = {
    val firstBytes = hexToBytes(firstHex)
    val secondBytes = hexToBytes(secondHex)
    val xor = xorBytes(firstBytes, secondBytes)
    bytesToHex(xor)
  }

  // Challenge 3
  def singleByteXorCipher(hexCipher: String): SingleCharXorResult = {
    decodeSingleCharXor(hexToBytes(hexCipher))
  }

  // Challenge 4
  def detectSingleCharXor(options: Seq[String]): SingleCharXorResult = {
    val results = for (option <- options)
      yield decodeSingleCharXor(hexToBytes(option))
    val res = results.minBy(_.score)
    res
  }

  // Challenge 5
  def repeatingKeyXor(message: String, key: String): String = {
    bytesToHex(xorWithKey(message.getBytes(), key.getBytes()))
  }

  // Challenge 6
  def breakRepeatingKeyXor(bytes: Seq[Byte], keySizeNum: Int): Seq[DecodedResult] = {
    val sortedKeys = getSortedXorVigenereKeySizes(bytes).take(keySizeNum)
    sortedKeys.map(_.keySize).map(keySize => {
      val key = getXorVigenereKey(keySize, bytes)
      DecodedResult(key, xorWithKey(bytes, key.getBytes()).map(_.toChar).mkString)
    })
  }

  // Challenge 7
  def breakAesInEcbMode(cipherText: Seq[Byte], key: String): String = {
    "TO DO"
  }
}