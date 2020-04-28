import org.scalatest._
import cryptopals.set1.Set1


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
    assert (set1.hexToBase64(input) == output)
  }

  "bytesToHex" should "work" in {
    assert(set1.bytesToHex(Array(12.toByte, 13.toByte)) == "0c0d")
  }

  "fixedXor" should "succeed" in {
    val input1 = "1c0111001f010100061a024b53535009181c"
    val input2 = "686974207468652062756c6c277320657965"
    val output = "746865206b696420646f6e277420706c6179"
    assert (set1.fixedXor(input1, input2) == output)
  }
}