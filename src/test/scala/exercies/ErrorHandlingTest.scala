package exercies

import exercies.ErrorHandling._
import org.scalatest.FunSpec

class ErrorHandlingTest extends FunSpec {

  describe("IOption") {

    describe("map") {
      it("does nothing with empty value") {
        assert(Option.empty[Int].map(_ => sys.error("gotcha")) == None)
      }

      it("maps contained value") {
        assert(Option(1).map(_.toString) == Some("1"))
      }

      /**
        * It might look a bit weird, but that's stdlib Option behaviour
        */
      it("does not mangle with return value") {
        assert(Option(1).map(_ => null) == Some(null))
      }
    }

    describe("flatMap") {
      it("does nothing with empty value") {
        assert(Option.empty[Int].flatMap(_ => sys.error("gotcha")) == None)
      }

      it("chains options together") {
        assert(Option(1).flatMap(x => Option(x + 1)) == Some(2))
        assert(Option(1).flatMap(_ => None) == None)
      }
    }

    describe("getOrElse") {
      it("gets default value if nothing is present") {
        assert(Option.empty[Int].getOrElse(42) == 42)
      }

      it("returns contents if present") {
        assert(Option(42).getOrElse(41) == 42)
      }
    }

    describe("orElse") {
      it("returns current option if present") {
        assert(Option(42).orElse(Option(36)) == Some(42))
      }

      it("returns alternative if no value") {
        assert(Option.empty[Int].orElse(Option(42)) == Some(42))
      }
    }

    describe("filter") {
      it("keeps value if filter is true") {
        assert(Option(42).filter(_ % 2 == 0) == Some(42))
      }

      it("discards value if filter is false") {
        assert(Option(42).filter(_ % 2 == 1) == None)
      }
    }
  }

  describe("mean") {
    it("can handle empty seq") {
      assert(mean(Nil) == None)
    }

    it("computes mean of non-empty seq") {
      assert(mean(Seq(1, 2, 3, 4)) == Some(2.5))
    }
  }

  describe("variance") {
    it("can handle empty seq") {
      assert(variance(Nil) == None)
    }

    it("computes variance of non-empty seq") {
      assert(variance(Seq(1, 2, 3, 4, 5, 6, 7)) == Some(4))
    }
  }

}
