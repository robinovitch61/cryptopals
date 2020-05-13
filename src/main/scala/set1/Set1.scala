package set1
import java.math.BigInteger
import java.util.Base64

case class SingleCharXorResult(encoded: Seq[Byte], xorNum: Long, text: String, score: Double)
case class KeyScore(keySize: Int, score: Double)
case class DecodedResult(key: String, text: String)

class Set1 {
  val alphabetFreqMap = Map(
    ('a', 8.497 / 100),
    ('b', 1.492 / 100),
    ('c', 2.202 / 100),
    ('d', 4.253 / 100),
    ('e', 11.16 / 100),
    ('f', 2.228 / 100),
    ('g', 2.015 / 100),
    ('h', 6.094 / 100),
    ('i', 7.546 / 100),
    ('j', 0.153 / 100),
    ('k', 1.292 / 100),
    ('l', 4.025 / 100),
    ('m', 2.406 / 100),
    ('n', 6.749 / 100),
    ('o', 7.507 / 100),
    ('p', 1.929 / 100),
    ('q', 0.095 / 100),
    ('r', 7.587 / 100),
    ('s', 6.327 / 100),
    ('t', 9.356 / 100),
    ('u', 2.758 / 100),
    ('v', 0.978 / 100),
    ('w', 2.560 / 100),
    ('x', 0.150 / 100),
    ('y', 1.994 / 100),
    ('z', 0.077 / 100),
  )

  // sum of squared frequences of the lowercase letters in english language
  // https://www.youtube.com/watch?v=kfR1i4EKpos
  val alphabetMetric = alphabetFreqMap.map(freq => math.pow(freq._2, 2)).sum

  val MinKeySize = 2
  val MaxKeySize = 40

  val MinAsciiPrintable = 32
  val MaxAsciiPrintable = 126
  val OtherAcceptableChars = Seq(
    10, // '\n'
  )
  val ReasonableNumCharsBeforeSpace = 8
  val SpecialCharThreshold = 8
  val SpecialCharAsciiSet = Set.range(33, 48).map(_.toChar) ++ Set.range(123, 127).map(_.toChar)

  def returns1 = () => 1

  def hexToBytes(hex: String) = new BigInteger(hex, 16).toByteArray.toSeq

  def bytesToBase64(bytes: Seq[Byte]): String = Base64.getEncoder().encodeToString(bytes.toArray)

  def base64ToBytes(encodedBase64: String): Seq[Byte] = {
    val cleaned = encodedBase64.map{ case '-' => '+'; case '_' => '/'; case c => c }
    Base64.getDecoder().decode(cleaned)
  }

  def bytesToHex(bytes: Seq[Byte]): String = {
    val sb = new StringBuilder
    for (b <- bytes) {
      sb.append(String.format("%02x", b))
    }
    sb.toString
  }

  def hexToBase64(hex: String): String = {
    bytesToBase64(hexToBytes(hex))
  }

  def fixedXor(first: String, second: String): String = {
    val firstBytes = hexToBytes(first)
    val secondBytes = hexToBytes(second)
    val xor = firstBytes.zip(secondBytes).map{ case (a, b) => (a ^ b).toByte }
    bytesToHex(xor)
  }

  def xorWithChar(bytes: Seq[Byte], char: Char): Seq[Byte] = {
    bytes.map( b => (b ^ char).toByte )
  }

  def charFrequency(message: String, char: Char): Long = {
    def helper(message: String, char: Char, count: Long): Long = {
      if (message.isEmpty) count else {
        message.head match {
          case `char` => helper(message.tail, char, count + 1)
          case _ => helper(message.tail, char, count)
        }
      }
    }
    helper(message, char, 0)
  }

  def numCharsInSet(message: String, set: Set[Char]): Long = {
    def helper(message: String, set: Set[Char], count: Long): Long = {
      if (message.isEmpty) {
        count
      } else if (set.contains(message.head)) {
        helper(message.tail, set, count + 1)
      } else {
        helper(message.tail, set, count)
      }
    }
    helper(message, set, 0)
  }

  def allCharsInAsciiRange(message: String): Boolean = {
    require(message.length > 0)
    def helper(message: String): Boolean = {
      if (message.isEmpty) true else {
        val firstChar = message.head.toInt
        if ((firstChar > MaxAsciiPrintable
          | firstChar < MinAsciiPrintable)
          & !OtherAcceptableChars.contains(firstChar)) {
          false
        } else {
          helper(message.tail)
        }
      }

    }
    helper(message)
  }

  def hasReasonableNumberOfSpaces(message: String): Boolean = {
    // Reasoning here is that an english message should contain some spaces
    // unfortunately this doesn't work very well as every n'th char of some message
    // may not contain many spaces and this is the premise of breaking xor Vigenere
    if (message.length < 8) true else {
      val spaceFreq = charFrequency(message, ' ').toDouble / message.length
      if (spaceFreq < (1.0 / ReasonableNumCharsBeforeSpace)) false else true
    }
  }

  def hasReasonableNumSpecialChars(message: String): Boolean = {
    // This should be better than space frequency as no matter if you're
    // taking every n'th char in a message, it would be weird if they were
    // disproportionately in (!, ", #, $, %...)
    if (message.length < 5) true else {
      val specialCharFreq = numCharsInSet(message, SpecialCharAsciiSet).toDouble / message.length
      if (specialCharFreq > (1.0 / SpecialCharThreshold)) false else true
    }
  }

  /**
   Computes the sum of squares of the character frequencies
   between ASCII characters [97, 122] (a-z lowercase)

   More plain english messages will have lower scores.
   */
  def frequencyScore(message: String): Double = {
    if (allCharsInAsciiRange(message) & hasReasonableNumSpecialChars(message)) {
      val chars = ('a' to 'z')
      val filteredMessage = message.replaceAll("[^a-z]","")
      val sumOfSquareFrequencies = chars.map(char => {
        math.pow(charFrequency(filteredMessage, char).toDouble / filteredMessage.length(), 2)
      }).sum
      math.abs(sumOfSquareFrequencies - alphabetMetric)
    } else {
      Double.MaxValue
    }
  }

  def chiSquaredScore(message: String): Double = {
    if (allCharsInAsciiRange(message) & hasReasonableNumSpecialChars(message)) {
      ('a' to 'z').map(char => {
        val expectedFreq = alphabetFreqMap(char) * message.length
        val messageFreq = charFrequency(message, char)
        math.pow(messageFreq - expectedFreq, 2) / expectedFreq
      }).sum
    } else {
      Double.MaxValue
    }
  }

  def decodeSingleCharXor(byteMessage: Seq[Byte]): SingleCharXorResult = {
    val results = for (i <- MinAsciiPrintable to MaxAsciiPrintable)
      yield {
        val decoded = xorWithChar(byteMessage, i.toChar).map(_.toChar).mkString
        SingleCharXorResult(byteMessage, i, decoded, frequencyScore(decoded))
      }
//    println(results.sortBy(_.score).take(5).map(char => (char.xorNum.toChar, char.text)))
    results.minBy(_.score)
  }

  def buildRepeatedKey(key: String, length: Int): String = {
    val repeatedKey = new StringBuilder
    while (repeatedKey.length() < length) {
      repeatedKey.append(key)
    }
    repeatedKey.substring(0, length)
  }

  def xorWithKey(input: Seq[Byte], key: String): Seq[Byte] = {
    val repeatedKey = buildRepeatedKey(key, input.size)
    input.zip(repeatedKey).map{ case (a, b) => (a ^ b).toByte }
  }

  def encodeToHexWithXorVigenere(message: String, key: String): String = {
    bytesToHex(xorWithKey(message.getBytes(), key))
  }

  def hammingDistance(first: Seq[Byte], second:Seq[Byte]): Int = {
    assert(first.length == second.length)
    first.zip(second).map{ case (a, b) => (a ^ b).toBinaryString.count(_ == '1') }.sum
  }

  def normalizedHammingDistance(first: Seq[Byte], second: Seq[Byte]): Double = {
    hammingDistance(first, second).toDouble / first.size
  }

  def getChunk[T](seq: Seq[T], chunkSize: Int, chunkNum: Int): Seq[T] = {
    // first chunk index 1
    assert(chunkSize * chunkNum <= seq.size)
    seq.slice(chunkSize * (chunkNum - 1), chunkSize * chunkNum)
  }

  def formattedBinaryString(bytes: Seq[Byte], length: Int): String = {
    String.format(s"%${length}s", bytes.map(_.toBinaryString).mkString).replace(' ', '0')
  }

  def keySizeScore(bytes: Seq[Byte], keySize: Int): Double = {
    // https://crypto.stackexchange.com/a/8118
    // require some minimum number of chunks to average
    if (keySize * 8 > bytes.size) {
      Double.MaxValue
    } else {

//      val firstChunk = getChunk(bytes, keySize, 1)
//      val secondChunk = getChunk(bytes, keySize, 2)
//      normalizedHammingDistance(firstChunk, secondChunk)

      var count = 1
      var score = 0.0
      val numChunks = math.floor(bytes.size / keySize).toInt
      // average pair-wise keySize-length groups of bytes' normalized hamming distance
      while (count < numChunks - 1) {
        val firstChunk = getChunk(bytes, keySize, count)
        val secondChunk = getChunk(bytes, keySize, count + 1)
        score = score + normalizedHammingDistance(firstChunk, secondChunk)
        count += 1
      }
      score / count
    }
  }

  def getEveryNthElement[T](seq: Seq[T], n: Int) = {
    // https://stackoverflow.com/a/25227836
    require(n > 0)
    for (step <- Range(start=0, end=seq.length, step=n))
      yield seq(step)
  }

  def getSortedXorVigenereKeySizes(encodedBytes: Seq[Byte]): Seq[KeyScore] = {
    (MinKeySize to MaxKeySize).map(keySize => {
      KeyScore(keySize, keySizeScore(encodedBytes, keySize))
    }).sortBy(_.score)
  }

  def getXorVigenereKey(keySize: Int, encodedBytes: Seq[Byte]): String = {
    (0 until keySize).map(offset => {
      val droppedBytes = encodedBytes.drop(offset)
      val block = getEveryNthElement(droppedBytes, keySize)
      decodeSingleCharXor(block).xorNum.toChar
    }).mkString
  }


  def breakXorVigenere(bytes: Seq[Byte], keySizeNum: Int = 3): Seq[DecodedResult] = {
//    println(getSortedXorVigenereKeySizes(bytes).mkString("\n"))
    val sortedKeys = getSortedXorVigenereKeySizes(bytes).take(keySizeNum)
    sortedKeys.map(_.keySize).map(keySize => {
      val key = getXorVigenereKey(keySize, bytes)
      DecodedResult(key, xorWithKey(bytes, key).map(_.toChar).mkString)
    })
  }
}