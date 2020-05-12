import org.scalatest._
import set1.Set1
import scala.io.Source


class Set1Spec extends FlatSpec {
  val set1 = new Set1()

  "returns1" should "return 1" in {
    assert (set1.returns1() == 1)
  }

  "hexToBytes" should "run correctly" in {
    assert(set1.hexToBytes("0110").mkString(" ") == "1 16")
  }

  "bytesToBase64" should "run correctly" in {
    assert(set1.bytesToBase64(Array(12.toByte, 13.toByte)) == "DA0=")
  }

  "hexToBase64" should "convert example correctly" in {
    val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    assert (set1.hexToBase64(input) == output)
  }

  "bytesToHex" should "work" in {
    assert(set1.bytesToHex(Array(12.toByte, 13.toByte)) == "0c0d")
  }

  "fixedXor" should "succeed" in {
    val input1 = "1c0111001f010100061a024b53535009181c"
    val input2 = "686974207468652062756c6c277320657965"
    val output = "746865206b696420646f6e277420706c6179"
    assert (set1.fixedXor(input1, input2) == output)
  }

  "decodeSingleCharXor" should "find the secret message" in {
    val hexCode = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    println("\n" + set1.decodeSingleCharXor(hexCode).text)
  }

  it should "find the secret message in all the options" in {
    val bufferedSource = Source.fromFile("src/main/scala/set1/set1_challenge4.txt")
    val results = for (line <- bufferedSource.getLines) yield set1.decodeSingleCharXor(line)
    println("\n" + results.minBy(_.score).text)
    bufferedSource.close
  }

  "encodeToHexWithXorVigenere" should "encode correctly" in {
    val unencrypted = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val key = "ICE"
    val output = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    assert(set1.encodeToHexWithXorVigenere(unencrypted, key) == output)
  }

  "hammingDistance" should "be 37 in the example given" in {
    val first = "this is a test"
    val second = "wokka wokka!!!"
    assert(set1.hammingDistance(first.getBytes(), second.getBytes()) == 37)
  }

  "normalizedHammingDistance" should "be 37 / 14 in the example given" in {
    val first = "this is a test"
    val second = "wokka wokka!!!"
    assert(set1.normalizedHammingDistance(first.getBytes(), second.getBytes()) == 37.0 / 14)
  }

  "getChunk" should "work" in {
    val text = "abcdefg"
    assert(set1.getChunk(text, 2, 1).mkString == "ab")
    assert(set1.getChunk(text, 2, 2).mkString == "cd")
    assert(set1.getChunk(text, 3, 2).mkString == "def")
  }

  "getEveryNthElement" should "work" in {
    val text = "abcdefg"
    assert(set1.getEveryNthElement(text, 1).mkString == text)
    assert(set1.getEveryNthElement(text, 2).mkString == "bdf")
    assert(set1.getEveryNthElement(text, 3).mkString == "cf")
    assert(set1.getEveryNthElement(text, 4).mkString == "d")
    assert(set1.getEveryNthElement(text, 5).mkString == "e")
    assert(set1.getEveryNthElement(text, 6).mkString == "f")
    assert(set1.getEveryNthElement(text, 7).mkString == "g")
  }

  "encodeWithXorVigenere" should "be 4 in a contrived example with keysize 4" in {
//    val plainText = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
//    val key = "ICE"
//    val encodedBase64 = set1.hexToBase64(set1.encodeToHexWithXorVigenere(plainText, key))
    val bufferedSource = Source.fromFile("src/main/scala/set1/set1_challenge6.txt")
    val encodedBase64 = bufferedSource.getLines.mkString


    val encodedBytes = set1.base64ToBytes(encodedBase64)
    println(encodedBase64)

    val sortedKeys = set1.getXorVigenereKeySize(encodedBytes)
    println(sortedKeys mkString "\n")
//    assert(set1.getXorVigenereKeySize(encodedBase64) == 4)

    sortedKeys.map(_.keySize).map(keySize => {
      println(set1.getXorVigenereKey(keySize, encodedBytes))
    })
  }

  "breakXorVigenere" should "break the code" in {

  }
}