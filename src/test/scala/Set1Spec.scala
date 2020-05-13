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
    assert(set1.hexToBase64(input) == output)
  }

  "bytesToHex" should "work" in {
    assert(set1.bytesToHex(Array(12.toByte, 13.toByte)) == "0c0d")
  }

  "allCharsInAsciiRange" should "work correctly" in {
    assert(set1.allCharsInAsciiRange("Now that the party is jumping"))
    assert(set1.allCharsInAsciiRange("Now that the party is jumping" + set1.MaxAsciiPrintable.toChar))
    assert(set1.allCharsInAsciiRange("Now that the party is jumping" + set1.MinAsciiPrintable.toChar))
    assert(!set1.allCharsInAsciiRange("Now that the party is jumping" + (set1.MaxAsciiPrintable + 1).toChar))
    assert(!set1.allCharsInAsciiRange("Now that the party is jumping" + (set1.MinAsciiPrintable - 1).toChar))
  }

  "hasReasonableNumberOfSpaces" should "work correctly" in {
    val good = "A reasonable number of spaces in a sentence"
    val bad = "Anunreasonableamountoftextbeforeaspace andsomemore"
    assert(set1.hasReasonableNumberOfSpaces(good))
    assert(!set1.hasReasonableNumberOfSpaces(bad))
  }

  "hasReasonableNumSpecialChars" should "work correctly" in {
    val good = "A message with a r$%easonable number of !"
    val bad = "T$#%oo M#!)ny!!!!!"
    val alsoBad = "t;871=;9t7tt<;t:!71tt' f'8tt?x^; 8s1;8t1t&80 '$5't-55:0:;!t&';397 =-x51;t0;;t7 t5t?t 2x2#;=2tt't51 "
    assert(set1.hasReasonableNumSpecialChars(good))
    assert(!set1.hasReasonableNumSpecialChars(bad))
    assert(!set1.hasReasonableNumSpecialChars(alsoBad))
  }

  "frequencyScore" should "be smaller for more englishey things" in {
    val best = "Hi there this is real english language!"
    val worst = "asdfahgfdslkasdjgkljasdhflkjadsf;lkajsdf;lkajsd;glkja;dslfjk"
    assert(set1.frequencyScore(best) < set1.frequencyScore(worst))
  }

  "fixedXor" should "succeed" in {
    val input1 = "1c0111001f010100061a024b53535009181c"
    val input2 = "686974207468652062756c6c277320657965"
    val output = "746865206b696420646f6e277420706c6179"
    assert(set1.fixedXor(input1, input2) == output)
  }

  "charFrequency" should "work correctly" in {
    assert(set1.charFrequency("This has two", ' ') == 2)
    assert(set1.charFrequency("Thishas one", ' ') == 1)
    assert(set1.charFrequency("Thishasnone", ' ') == 0)
    val weird = Vector(119, 107, 34, 41, 99, 106, 100, 34, 116, 102, 100, 37, 104, 96, 37, 37, 100, 37, 96, 76, 96, 37, 102, 103).map(_.toChar).mkString
    assert(set1.charFrequency(weird, ' ') == 0)
  }

  "numCharsInSet" should "work correctly" in {
    val message = "This is a message"
    val set = Set('m', 's')
    assert(set1.numCharsInSet(message, set) == 5)
  }

  "decodeSingleCharXor" should "find the secret message" in {
    val hexCode = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    val result = set1.decodeSingleCharXor(set1.hexToBytes(hexCode))
    println("\n" + result.xorNum.toChar + "\n" + result.text)
    val encodedBytes = set1.xorWithChar(result.text.getBytes, result.xorNum.toChar)
    val encodedHex = set1.bytesToHex(encodedBytes)
    assert(encodedHex == hexCode)
  }

  it should "find the secret message in all the options" in {
    val bufferedSource = Source.fromFile("src/main/scala/set1/set1_challenge4.txt")
    val results = for (line <- bufferedSource.getLines)
      yield set1.decodeSingleCharXor(set1.hexToBytes(line))
    val likelyResult = results.minBy(_.score)
    println("\n" + likelyResult.xorNum.toChar + "\n" + likelyResult.text)

    val encodedBytes = set1.xorWithChar(likelyResult.text.getBytes, likelyResult.xorNum.toChar)
    val encodedHex = set1.bytesToHex(encodedBytes)
    assert(encodedHex == set1.bytesToHex(likelyResult.encoded))

    bufferedSource.close
  }

  "buildRepeatedKey" should "work" in {
    val key = "ICE"
    val message = "ICEICEBABY"
    val repeatedKey = set1.buildRepeatedKey(key, message.length)
    assert(repeatedKey == "ICEICEICEI")
  }

  "xorWithKey" should "be reversible" in {
    val key = "ICE"
    val message = "This should be the same after being xor'd with the key twice"
    val encodedOnce = set1.xorWithKey(message.getBytes(), key)
    val encodedTwice = set1.xorWithKey(encodedOnce, key)
    val stringified = encodedTwice.map(_.toChar).mkString
    assert(stringified == message)
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
    assert(set1.getEveryNthElement(text, 2).mkString == "aceg")
    assert(set1.getEveryNthElement(text, 3).mkString == "adg")
    assert(set1.getEveryNthElement(text, 4).mkString == "ae")
    assert(set1.getEveryNthElement(text, 5).mkString == "af")
    assert(set1.getEveryNthElement(text, 6).mkString == "ag")
    assert(set1.getEveryNthElement(text, 7).mkString == "a")
    assert(set1.getEveryNthElement(text, 8).mkString == "a")
  }

  "breakXorVigenere" should "decode the problem's message" in {
    val bufferedSource = Source.fromFile("src/main/scala/set1/set1_challenge6.txt")
    val encodedBase64 = bufferedSource.getLines.mkString
    val encodedBytes = set1.base64ToBytes(encodedBase64)
    val results = set1.breakXorVigenere(encodedBytes, 1)
    results.map(res => println("\nkeySize: " + res.key.length + "\nKey: " + res.key + "\n" + res.text.take(100) + "\n"))
  }

  "breakXorVigenere" should "decode the vanilla ice message" in {
    val plainText = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val key = "ICE"
    val encodedHex = set1.encodeToHexWithXorVigenere(plainText, key)
    val encodedBase64 = set1.hexToBase64(encodedHex)
    val encodedBytes = set1.base64ToBytes(encodedBase64)
    val results = set1.breakXorVigenere(encodedBytes, 3)
    results.map(res => println("\nkeySize: " + res.key.length + "\nKey: " + res.key + "\n" + res.text.take(100) + "\n"))
  }

  "breakXorVigenere" should "decode a custom message" in {
    val plainText = "There are few things as satisfying as a cold drink of water on a hot summer's day. The longer this text is, the better the result. There are some improvements to be made to both the algorithm scoring English-like-ness of things and the keySize determination!"
    val key = "leo"
    println("Real keySize: " + key.length)
    val encodedHex = set1.encodeToHexWithXorVigenere(plainText, key)
    val encodedBase64 = set1.hexToBase64(encodedHex)
    val encodedBytes = set1.base64ToBytes(encodedBase64)
    val results = set1.breakXorVigenere(encodedBytes, 40)
    results.map(res => println("\nkeySize: " + res.key.length + "\nKey: " + res.key + "\n" + res.text.take(100) + "\n"))
  }
}