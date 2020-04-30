package set1
import java.math.BigInteger
import java.util.Base64

case class Result(num: Long, text: String, score: Double)

class Set1 {
  import Set1.alphabetMetric

  def returns1 = () => 1

  def hexToBytes(hex: String) = new BigInteger(hex, 16).toByteArray

  def bytesToBase64(bytes: Array[Byte]): String = Base64.getEncoder().encodeToString(bytes)

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

  def xorWithChar(bytes: Array[Byte], char: Char): String = {
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

  def decodeSingleCharXor(hexMessage: String): Result = {
    val byteCode = hexToBytes(hexMessage)

    val results = for (i <- 33 until 128)
      yield {
        val decoded = xorWithChar(byteCode, i.toChar)
        Result(i, decoded, frequencyScore(decoded))
      }

    results.minBy(_.score)
  }

  def encodeWithKeyXor(message: String, key: String): String = {
    val repeatedKey = new StringBuilder
    while (repeatedKey.length() < message.length()) {
      repeatedKey.append(key)
    }
    bytesToHex(message.zip(repeatedKey).map{ case (a, b) => (a ^ b).toByte })
  }

  def hammingDistance(first: String, second:String): Int = {
    assert(first.length == second.length)
    first.zip(second).map{ case (a, b) => (a ^ b).toBinaryString.count(_ == '1') }.sum
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
  val alphabetMetric = alphabetFreqMap.map(freq => math.pow(freq._2, 2)).sum
}
