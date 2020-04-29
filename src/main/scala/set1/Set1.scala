package set1

import java.math.BigInteger
import java.util.Base64

class Set1 {
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

  def isMaybeAMessage(message: String): Boolean = {
//    // Calculate the percentage of spaces in the line
//    // Normal range ~[5, 20]
//    val invalidChars = Array("\n", '[', ']', '~', '>', '<', '|')
//    val numSpaces = charFrequency(message, ' ')
//    val percentSpaces = numSpaces.toDouble / message.length * 100
//    val validPercentSpaces = percentSpaces <= 20 && percentSpaces >= 5
//    validPercentSpaces && !message.contains(invalidChars)

    // Better approach - percent of "normal" chars
    val validChars = (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ List(' ')).toSet
    val numValidChars = numCharsInSet(message, validChars)
    numValidChars.toDouble / message.length() * 100 > 90
  }

  def decodeSingleCharXor(hexMessage: String): Unit = {
    val byteCode = hexToBytes(hexMessage)

    case class Result(num: Long, text: String)
    val results = for (i <- 33 until 128)
      yield Result(i, xorWithChar(byteCode, i.toChar))

    results.foreach(r => if (isMaybeAMessage(r.text.mkString)) {
      println(s"\n${r.num}, ${r.num.toChar}, ${r.text.mkString}\n")
    })
  }

  def encodeWithKeyXor(message: String, key: String): String = {
    val repeatedKey = new StringBuilder
    while (repeatedKey.length() < message.length()) {
      repeatedKey.append(key)
    }
    bytesToHex(message.zip(repeatedKey).map{ case (a, b) => (a ^ b).toByte })
  }
}
