package set1
import java.math.BigInteger
import java.util.Base64

case class SingleCharXorResult(xorNum: Long, text: String, score: Double)
case class KeyScore(keySize: Int, score: Double)

class Set1 {
  import Set1.{alphabetMetric, MinKeySize, MaxKeySize}

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

  def xorWithChar(bytes: Seq[Byte], char: Char): String = {
    bytes.map( b => (b ^ char.toByte).toChar ).mkString
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

  /**
   Computes the sum of squares of the character frequencies
   between ASCII characters [97, 122] (a-z lowercase)

   More plain english messages will have lower scores.
   */
  def frequencyScore(message: String): Double = {
    val chars = ('a' to 'z')
    val sumOfSquareFrequencies = chars.map(char => {
        math.pow(charFrequency(message, char).toDouble / message.length(), 2)
      }).sum
    math.abs(sumOfSquareFrequencies - alphabetMetric)
  }

  def decodeSingleCharXor(hexMessage: String): SingleCharXorResult = {
    val byteCode = hexToBytes(hexMessage)

    val results = for (i <- 33 until 128)
      yield {
        val decoded = xorWithChar(byteCode, i.toChar)
        SingleCharXorResult(i, decoded, frequencyScore(decoded))
      }

    results.minBy(_.score)
  }

  def buildRepeatedKey(key: String, length: Int): String = {
    val repeatedKey = new StringBuilder
    while (repeatedKey.length() < length) {
      repeatedKey.append(key)
    }
    repeatedKey.substring(0, length)
  }

  def encodeToHexWithXorVigenere(message: String, key: String): String = {
    val repeatedKey = buildRepeatedKey(key, message.length())
    bytesToHex(message.zip(repeatedKey).map{ case (a, b) => (a ^ b).toByte })
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

  def keySizeScore(bytes: Seq[Byte], keySize: Int): Double = {
    if (keySize * 4 > bytes.size) {
      Double.MaxValue
    } else {
      val firstChunk = getChunk(bytes, keySize, 1)
      val secondChunk = getChunk(bytes, keySize, 2)
      val thirdChunk = getChunk(bytes, keySize, 3)
      val fourthChunk = getChunk(bytes, keySize, 4)

      val score = normalizedHammingDistance(firstChunk, secondChunk)
//      val score = (
//        hammingDistance(firstChunk, secondChunk).toDouble / keySize
//        + hammingDistance(firstChunk, thirdChunk).toDouble / keySize
//        + hammingDistance(firstChunk, fourthChunk).toDouble / keySize
//        + hammingDistance(secondChunk, thirdChunk).toDouble / keySize
//        + hammingDistance(secondChunk, fourthChunk).toDouble / keySize
//        + hammingDistance(thirdChunk, fourthChunk).toDouble / keySize
//      ) / 6

//      println()
//      println(keySize)
//      println(firstChunk, secondChunk)
//      println(thirdChunk, fourthChunk)
//      println(score)
      score
    }
  }

  def getEveryNthElement[T](seq: Seq[T], n: Int) = {
    // https://stackoverflow.com/a/25227836
    require(n > 0)
    for (step <- Range(start=n - 1, end=seq.length, step=n))
      yield seq(step)
  }

  def getXorVigenereKeySize(encodedBytes: Seq[Byte]): Seq[KeyScore] = {
    (MinKeySize to MaxKeySize).map(keySize => {
      KeyScore(keySize, keySizeScore(encodedBytes, keySize))
    }).sortBy(_.score)
  }
  
  def getXorVigenereKey(keySize: Int, encodedBytes: Seq[Byte]): String = {
    (1 to keySize).map(offset => {
      val block = getEveryNthElement(encodedBytes, offset)
      val hexBlock = bytesToHex(block)
      decodeSingleCharXor(hexBlock).xorNum.toChar
    }).toString
  }

  def breakXorVigenere(encodedBase64: String): String = {
    // https://crypto.stackexchange.com/a/8118
    val encodedBytes = base64ToBytes(encodedBase64)
    println(encodedBytes)
    val sortedKeySizes = getXorVigenereKeySize(encodedBytes)

    "abc"
  }
}

object Set1 {
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
  val MaxKeySize = 10
}
