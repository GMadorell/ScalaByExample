import org.scalatest.{Matchers, FlatSpec}

class FactorialTest extends FlatSpec with Matchers {

  "head recursive factorial" should "return correct values" in {
    Factorial.headRecursive(1) should be (1)
    Factorial.headRecursive(2) should be (2)
    Factorial.headRecursive(3) should be (6)
    Factorial.headRecursive(4) should be (24)
    Factorial.headRecursive(10) should be (3628800)
  }

  "tail recursive factorial" should "return correct values" in {
    Factorial.tailRecursive(1) should be (1)
    Factorial.tailRecursive(2) should be (2)
    Factorial.tailRecursive(3) should be (6)
    Factorial.tailRecursive(4) should be (24)
    Factorial.tailRecursive(10) should be (3628800)
  }
}
