import org.scalatest._
import cryptopals.helpers.CryptoHelpers
import scala.io.Source


class CryptoHelpersSpec extends FlatSpec {
  val helpers = new CryptoHelpers()

  "returns1" should "return 1" in {
    assert (helpers.returns1() == 1)
  }

  "hexToBytes" should "run correctly" in {
    assert(helpers.hexToBytes("0110").mkString(" ") == "1 16")
  }

  "bytesToBase64" should "run correctly" in {
    assert(helpers.bytesToBase64(Array(12.toByte, 13.toByte)) == "DA0=")
  }

  "hexToBase64" should "convert example correctly" in {
    val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    assert(helpers.hexToBase64(input) == output)
  }

  "bytesToHex" should "work" in {
    assert(helpers.bytesToHex(Array(12.toByte, 13.toByte)) == "0c0d")
  }

  "base64ToBytes" should "work" in {
    assert(helpers.base64ToBytes("dGVzdGluZw==").map(_.toChar).mkString == "testing")
  }

  "hammingDistance" should "be 37 in the example given" in {
    val first = "this is a test"
    val second = "wokka wokka!!!"
    assert(helpers.hammingDistance(first.getBytes(), second.getBytes()) == 37)
  }

  "normalizedHammingDistance" should "be 37 / 14 in the example given" in {
    val first = "this is a test"
    val second = "wokka wokka!!!"
    assert(helpers.normalizedHammingDistance(first.getBytes(), second.getBytes()) == 37.0 / 14)
  }

  "getChunk" should "work" in {
    val text = "abcdefg"
    assert(helpers.getChunk(text, 2, 1).mkString == "ab")
    assert(helpers.getChunk(text, 2, 2).mkString == "cd")
    assert(helpers.getChunk(text, 3, 2).mkString == "def")
  }

  "formattedBinaryString" should "work" in {
    assert(helpers.formattedBinaryString(Seq(1), 16) == "0000000000000001")
  }

  "xorBytes" should "work" in {
    // TODO
  }

  "xorWithChar" should "work" in {
    // TODO
  }

  "charFrequency" should "work correctly" in {
    assert(helpers.charFrequency("This has two", ' ') == 2)
    assert(helpers.charFrequency("Thishas one", ' ') == 1)
    assert(helpers.charFrequency("Thishasnone", ' ') == 0)
    assert(helpers.charFrequency("aaa", 'a') == 3)
    val weird = Vector(119, 107, 34, 41, 99, 106, 100, 34, 116, 102, 100, 37, 104, 96, 37, 37, 100, 37, 96, 76, 96, 37, 102, 103).map(_.toChar).mkString
    assert(helpers.charFrequency(weird, ' ') == 0)
  }

  "numCharsInSet" should "work correctly" in {
    val message = "This is a message"
    val set = Set('m', 's')
    assert(helpers.numCharsInSet(message, set) == 5)
  }

  "allCharsInAsciiRange" should "work correctly" in {
    assert(helpers.allCharsInAsciiRange("Now that the party is jumping"))
    assert(helpers.allCharsInAsciiRange("Now that the party is jumping" + helpers.MaxAsciiPrintable.toChar))
    assert(helpers.allCharsInAsciiRange("Now that the party is jumping" + helpers.MinAsciiPrintable.toChar))
    assert(!helpers.allCharsInAsciiRange("Now that the party is jumping" + (helpers.MaxAsciiPrintable + 1).toChar))
    assert(!helpers.allCharsInAsciiRange("Now that the party is jumping" + (helpers.MinAsciiPrintable - 1).toChar))
  }

  "hasReasonableNumberOfSpaces" should "work correctly" in {
    val good = "A reasonable number of spaces in a sentence"
    val bad = "Anunreasonableamountoftextbeforeaspace andsomemore"
    assert(helpers.hasReasonableNumberOfSpaces(good))
    assert(!helpers.hasReasonableNumberOfSpaces(bad))
  }

  "hasReasonableNumSpecialChars" should "work correctly" in {
    val good = "A message with a r$%easonable number of !"
    val bad = "T$#%oo M#!)ny!!!!!"
    val alsoBad = "t;871=;9t7tt<;t:!71tt' f'8tt?x^; 8s1;8t1t&80 '$5't-55:0:;!t&';397 =-x51;t0;;t7 t5t?t 2x2#;=2tt't51 "
    val bad3 = "{jm>>i>lvjwj{l{{>qyqw{p{zw"
    assert(helpers.hasReasonableNumSpecialChars(good))
    assert(!helpers.hasReasonableNumSpecialChars(bad))
    assert(!helpers.hasReasonableNumSpecialChars(alsoBad))
    assert(!helpers.hasReasonableNumSpecialChars(bad3))
  }

  "frequencyScore" should "be smaller for more englishey things" in {
    val best = "Hi there this is real english language!"
    val worst = "asdfahgfdslkasdjgkljasdhflkjadsf;lkajsdf;lkajsd;glkja;dslfjk"
    assert(helpers.frequencyScore(best) < helpers.frequencyScore(worst))
  }

  "buildRepeatedKey" should "work" in {
    val key = "ICE"
    val message = "ICEICEBABY"
    val repeatedKey = helpers.buildRepeatedKey(key.getBytes(), message.length)
    assert(repeatedKey.map(_.toChar).mkString == "ICEICEICEI")
  }

  "xorWithKey" should "be reversible" in {
    val key = "ICE"
    val message = "This should be the same after being xor'd with the key twice"
    val encodedOnce = helpers.xorWithKey(message.getBytes(), key.getBytes())
    val encodedTwice = helpers.xorWithKey(encodedOnce, key.getBytes())
    val stringified = encodedTwice.map(_.toChar).mkString
    assert(stringified == message)
  }

  "getEveryNthElement" should "work" in {
    val text = "abcdefg"
    assert(helpers.getEveryNthElement(text, 1).mkString == text)
    assert(helpers.getEveryNthElement(text, 2).mkString == "aceg")
    assert(helpers.getEveryNthElement(text, 3).mkString == "adg")
    assert(helpers.getEveryNthElement(text, 4).mkString == "ae")
    assert(helpers.getEveryNthElement(text, 5).mkString == "af")
    assert(helpers.getEveryNthElement(text, 6).mkString == "ag")
    assert(helpers.getEveryNthElement(text, 7).mkString == "a")
    assert(helpers.getEveryNthElement(text, 8).mkString == "a")
  }
}