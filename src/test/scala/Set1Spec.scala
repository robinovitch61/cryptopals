import org.scalatest._

import scala.io.Source

class Set1Spec extends FlatSpec {
  val set1 = new Set1()

  "convertHexToBase64" should "get the right answer" in {
    val hexString = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val base64String = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    assert(set1.convertHexToBase64(hexString) == base64String)
  }

  "fixedXor" should "get the right answer" in {
    val firstHex = "1c0111001f010100061a024b53535009181c"
    val secondHex = "686974207468652062756c6c277320657965"
    val resHex = "746865206b696420646f6e277420706c6179"
    assert(set1.fixedXor(firstHex, secondHex) == resHex)
  }

  "singleByteXorCipher" should "get the right answer" in {
    val hexCipher = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
    val res = set1.singleByteXorCipher(hexCipher)

    // print message to see if it makes sense
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(s"Challenge 3 Message: ${res.text}")
    println(s"Decoded With Char: ${res.xorNum.toChar}")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")

    // re-encode and ensure everything is correct
    val encodedBytes = set1.xorWithChar(res.text.getBytes, res.xorNum.toChar)
    val encodedHex = set1.bytesToHex(encodedBytes)
    assert(encodedHex.mkString == hexCipher)
  }

  "detectSingleCharXor" should "get the right answer" in {
    val options = Source.fromFile("src/main/scala/set1/set1_challenge4.txt").getLines().toSeq
    val res = set1.detectSingleCharXor(options)

    // print message to see if it makes sense
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(s"Challenge 4 Message: ${res.text}")
    println(s"Decoded With Char: ${res.xorNum.toChar}")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  }

  "repeatingKeyXor" should "get the right answer" in {
    val message = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    val key = "ICE"
    val res = set1.repeatingKeyXor(message, key)
    val output = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
    assert(res == output)
  }

  "breakXorVigenere" should "decode the problem's message" in {
    val bufferedSource = Source.fromFile("src/main/scala/set1/set1_challenge6.txt")
    val encodedBase64 = bufferedSource.getLines.mkString
    val encodedBytes = set1.base64ToBytes(encodedBase64)
    val results = set1.breakRepeatingKeyXor(encodedBytes, 1)

    // print message(s) to see if any make sense
    results.map(res => {
      println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
      println(s"Challenge 6 Message: ${res.text}")
      println(s"Encoded with Key: ${res.key}")
      println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    })
  }

//  // Some interesting Challenge 6 experiments below
//  def breakXorVigenereTester(message: String, key: String, numKeyLengths: Int, label: String): Unit = {
//    val encodedHex = set1.repeatingKeyXor(message, key)
//    val encodedBase64 = set1.hexToBase64(encodedHex)
//    val encodedBytes = set1.base64ToBytes(encodedBase64)
//    val results = set1.breakRepeatingKeyXor(encodedBytes, numKeyLengths)
//
//    // print message(s) to see if any make sense
//    results.map(res => {
//      println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
//      println(s"breakXorVigenere ${label} Message: ${res.text}")
//      println(s"Encoded with Key: ${res.key}")
//      println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
//    })
//  }
//
//  "breakXorVigenereTester" should "decode the vanilla ice message" in {
//    val message = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
//    val key = "ICE"
//    val label = "Vanilla Ice Test"
//    val numKeyLengths = 3
//    breakXorVigenereTester(message, key, numKeyLengths, label)
//  }
//
//  it should "decode the ascii text message" in {
//    val message = "ASCII stands for American Standard Code for Information Interchange. Computers can only understand numbers, so an ASCII code is the numerical representation of a character such as 'a' or '@' or an action of some sort. ASCII was developed a long time ago and now the non-printing characters are rarely used for their original purpose. Below is the ASCII character table and this includes descriptions of the first 32 non-printing characters. ASCII was actually designed for use with teletypes and so the descriptions are somewhat obscure. If someone says they want your CV however in ASCII format, all this means is they want 'plain' text with no formatting such as tabs, bold or underscoring - the raw format that any computer can understand. This is usually so they can easily import the file into their own applications without issues. Notepad.exe creates ASCII text, or in MS Word you can save a file as 'text only'\n"
//    val key = "akeythatisnice"
//    val label = "ASCII Test Text"
//    val numKeyLengths = 3
//    breakXorVigenereTester(message, key, numKeyLengths, label)
//  }
//
//  it should "decode the challenge message with a different key" in {
//    val message = "I'm back and I'm ringin' the bell\nA rockin' on the mike while the fly girls yell\nIn ecstasy in the back of me\nWell that's my DJ Deshay cuttin' all them Z's\nHittin' hard and the girlies goin' crazy\nVanilla's on the mike, man I'm not lazy.\n\nI'm lettin' my drug kick in\nIt controls my mouth and I begin\nTo just let it flow, let my concepts go\nMy posse's to the side yellin', Go Vanilla Go!\n\nSmooth 'cause that's the way I will be\nAnd if you don't give a damn, then\nWhy you starin' at me\nSo get off 'cause I control the stage\nThere's no dissin' allowed\nI'm in my own phase\nThe girlies sa y they love me and that is ok\nAnd I can dance better than any kid n' play\n\nStage 2 -- Yea the one ya' wanna listen to\nIt's off my head so let the beat play through\nSo I can funk it up and make it sound good\n1-2-3 Yo -- Knock on some wood\nFor good luck, I like my rhymes atrocious\nSupercalafragilisticexpialidocious\nI'm an effect and that you can bet\nI can take a fly girl and make her wet.\n\nI'm like Samson -- Samson to Delilah\nThere's no denyin', You can try to hang\nBut you'll keep tryin' to get my style\nOver and over, practice makes perfect\nBut not if you're a loafer.\n\nYou'll get nowhere, no place, no time, no girls\nSoon -- Oh my God, homebody, you probably eat\nSpaghetti with a spoon! Come on and say it!\n\nVIP. Vanilla Ice yep, yep, I'm comin' hard like a rhino\nIntoxicating so you stagger like a wino\nSo punks stop trying and girl stop cryin'\nVanilla Ice is sellin' and you people are buyin'\n'Cause why the freaks are jockin' like Crazy Glue\nMovin' and groovin' trying to sing along\nAll through the ghetto groovin' this here song\nNow you're amazed by the VIP posse.\n\nSteppin' so hard like a German Nazi\nStartled by the bases hittin' ground\nThere's no trippin' on mine, I'm just gettin' down\nSparkamatic, I'm hangin' tight like a fanatic\nYou trapped me once and I thought that\nYou might have it\nSo step down and lend me your ear\n'89 in my time! You, '90 is my year.\n\nYou're weakenin' fast, YO! and I can tell it\nYour body's gettin' hot, so, so I can smell it\nSo don't be mad and don't be sad\n'Cause the lyrics belong to ICE, You can call me Dad\nYou're pitchin' a fit, so step back and endure\nLet the witch doctor, Ice, do the dance to cure\nSo come up close and don't be square\nYou wanna battle me -- Anytime, anywhere\n\nYou thought that I was weak, Boy, you're dead wrong\nSo come on, everybody and sing this song\n\nSay -- Play that funky music Say, go white boy, go white boy go\nplay that funky music Go white boy, go white boy, go\nLay down and boogie and play that funky music till you die.\n\nPlay that funky music Come on, Come on, let me hear\nPlay that funky music white boy you say it, say it\nPlay that funky music A little louder now\nPlay that funky music, white boy Come on, Come on, Come on\nPlay that funky music"
//    val key = "a whole new keyyyy"
//    val label = "Different Key Test"
//    val numKeyLengths = 3
//    breakXorVigenereTester(message, key, numKeyLengths, label)
//  }

  "breakAesInEcbMode" should "get the right answer" in {
    val bufferedSource = Source.fromFile("src/main/scala/set1/set1_challenge7.txt")
    val encodedBase64 = bufferedSource.getLines.mkString
    val encodedBytes = set1.base64ToBytes(encodedBase64)
    val res = set1.breakAesInEcbMode(encodedBytes, "YELLOW SUBMARINE")
    assert(res != "TO DO")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
    println(s"Challenge 7 AES 128 IN ECB Message: ${res}")
    println("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~")
  }
}