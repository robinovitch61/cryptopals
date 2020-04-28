package cryptopals.set1
import java.util.Base64
import java.math.BigInteger

class Set1 {
  def returns1 = () => 1

  def hex_to_base64(hex: String): String = {
    val bytes = new BigInteger(hex, 16).toByteArray
    Base64.getEncoder().encodeToString(bytes)
  }
}
