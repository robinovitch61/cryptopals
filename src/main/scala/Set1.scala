package cryptopals.set1
import java.util.Base64
import java.math.BigInteger

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
}
