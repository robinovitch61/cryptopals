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
    val bad3 = "{jm>>i>lvjwj{l{{>qyqw{p{zw"
    assert(set1.hasReasonableNumSpecialChars(good))
    assert(!set1.hasReasonableNumSpecialChars(bad))
    assert(!set1.hasReasonableNumSpecialChars(alsoBad))
    assert(!set1.hasReasonableNumSpecialChars(bad3))
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
    results.map(res => println("\nkeySize: " + res.key.length + "\nKey: " + res.key + "\n" + res.text + "\n"))
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
    val plainText = "ASCII stands for American Standard Code for Information Interchange. Computers can only understand numbers, so an ASCII code is the numerical representation of a character such as 'a' or '@' or an action of some sort. ASCII was developed a long time ago and now the non-printing characters are rarely used for their original purpose. Below is the ASCII character table and this includes descriptions of the first 32 non-printing characters. ASCII was actually designed for use with teletypes and so the descriptions are somewhat obscure. If someone says they want your CV however in ASCII format, all this means is they want 'plain' text with no formatting such as tabs, bold or underscoring - the raw format that any computer can understand. This is usually so they can easily import the file into their own applications without issues. Notepad.exe creates ASCII text, or in MS Word you can save a file as 'text only'\n"
    val key = "asdfasdf"
    println("Real keySize: " + key.length)
    val encodedHex = set1.encodeToHexWithXorVigenere(plainText, key)
    val encodedBase64 = set1.hexToBase64(encodedHex)
    val encodedBytes = set1.base64ToBytes(encodedBase64)
    val results = set1.breakXorVigenere(encodedBytes, 10)
    results.map(res => println("\nkeySize: " + res.key.length + "\nKey: " + res.key + "\n" + res.text.take(100) + "\n"))
  }

  "breakXorVigenere" should "encrypt/decrypt the message with a new key properly" in {
    val plainText = "I'm back and I'm ringin' the bell\nA rockin' on the mike while the fly girls yell\nIn ecstasy in the back of me\nWell that's my DJ Deshay cuttin' all them Z's\nHittin' hard and the girlies goin' crazy\nVanilla's on the mike, man I'm not lazy.\n\nI'm lettin' my drug kick in\nIt controls my mouth and I begin\nTo just let it flow, let my concepts go\nMy posse's to the side yellin', Go Vanilla Go!\n\nSmooth 'cause that's the way I will be\nAnd if you don't give a damn, then\nWhy you starin' at me\nSo get off 'cause I control the stage\nThere's no dissin' allowed\nI'm in my own phase\nThe girlies sa y they love me and that is ok\nAnd I can dance better than any kid n' play\n\nStage 2 -- Yea the one ya' wanna listen to\nIt's off my head so let the beat play through\nSo I can funk it up and make it sound good\n1-2-3 Yo -- Knock on some wood\nFor good luck, I like my rhymes atrocious\nSupercalafragilisticexpialidocious\nI'm an effect and that you can bet\nI can take a fly girl and make her wet.\n\nI'm like Samson -- Samson to Delilah\nThere's no denyin', You can try to hang\nBut you'll keep tryin' to get my style\nOver and over, practice makes perfect\nBut not if you're a loafer.\n\nYou'll get nowhere, no place, no time, no girls\nSoon -- Oh my God, homebody, you probably eat\nSpaghetti with a spoon! Come on and say it!\n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino\nIntoxicating so you stagger like a wino\nSo punks stop trying and girl stop cryin'\nVanilla Ice is sellin' and you people are buyin'\n'Cause why the freaks are jockin' like Crazy Glue\nMovin' and groovin' trying to sing along\nAll through the ghetto groovin' this here song\nNow you're amazed by the VIP posse.\n\nSteppin' so hard like a German Nazi\nStartled by the bases hittin' ground\nThere's no trippin' on mine, I'm just gettin' down\nSparkamatic, I'm hangin' tight like a fanatic\nYou trapped me once and I thought that\nYou might have it\nSo step down and lend me your ear\n'89 in my time! You, '90 is my year.\n\nYou're weakenin' fast, YO! and I can tell it\nYour body's gettin' hot, so, so I can smell it\nSo don't be mad and don't be sad\n'Cause the lyrics belong to ICE, You can call me Dad\nYou're pitchin' a fit, so step back and endure\nLet the witch doctor, Ice, do the dance to cure\nSo come up close and don't be square\nYou wanna battle me -- Anytime, anywhere\n\nYou thought that I was weak, Boy, you're dead wrong\nSo come on, everybody and sing this song\n\nSay -- Play that funky music Say, go white boy, go white boy go\nplay that funky music Go white boy, go white boy, go\nLay down and boogie and play that funky music till you die.\n\nPlay that funky music Come on, Come on, let me hear\nPlay that funky music white boy you say it, say it\nPlay that funky music A little louder now\nPlay that funky music, white boy Come on, Come on, Come on\nPlay that funky music"
    val key = "a whole new keyyyy"
    println("Real keySize: " + key.length)
    val encodedHex = set1.encodeToHexWithXorVigenere(plainText, key)
    val encodedBase64 = set1.hexToBase64(encodedHex)
    val encodedBytes = set1.base64ToBytes(encodedBase64)
    val results = set1.breakXorVigenere(encodedBytes, 10)
    results.map(res => println("\nkeySize: " + res.key.length + "\nKey: " + res.key + "\n" + res.text.take(100) + "\n"))
  }
}