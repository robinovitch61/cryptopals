import org.scalatest._
import cryptopals.set1.Set1


class Set1Spec extends FlatSpec {
  val set1 = new Set1()

  "returns1" should "return 1" in {
    assert (set1.returns1() == 1)
  }

  "hex_to_base64" should "convert example correctly" in {
    val input = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    val output = "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
    assert (set1.hex_to_base64(input) == output)
  }
}