package exercies

import org.scalatest.FunSpec

class TsvTest extends FunSpec {
  case class Something(
    s: String,
    i: Int,
    b: Boolean
  )

  describe("TsvEncoder") {
    import Tsv._

    it("encodes a whole thing as tsv") {
      assert(
        Tsv.writeTsv(Something("string", 10, false) :: Nil) ==
        "string\t10\tfalse")
    }

    it("encodes multiple lines") {
      val lines = Something("string", 10, false) ::
        Something("else", 20, true) ::
        Nil

      assert(
        Tsv.writeTsv(lines) ==
          "string\t10\tfalse\n" +
            "else\t20\ttrue")

    }
  }
}
