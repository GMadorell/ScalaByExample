import org.scalatest.{Matchers, FlatSpec}

class SqrtTest extends FlatSpec with Matchers {
  val TestTolerance = 0.01

  it should "work well for small round results" in {
    Sqrt.sqrt(1) should be (1.0 +- TestTolerance)
    Sqrt.sqrt(4) should be (2.0 +- TestTolerance)
    Sqrt.sqrt(16) should be (4.0 +- TestTolerance)
    Sqrt.sqrt(100) should be (10.0 +- TestTolerance)
    Sqrt.sqrt(625) should be (25.0 +- TestTolerance)
  }

  it should "work well for small odd results" in {
    Sqrt.sqrt(2) should be (1.414 +- TestTolerance)
    Sqrt.sqrt(3) should be (1.732 +- TestTolerance)
    Sqrt.sqrt(10) should be (3.162 +- TestTolerance)
  }

  it should "work well for big round results" in {
    Sqrt.sqrt(25000000.0) should be (5000.0 +- TestTolerance)
    Sqrt.sqrt(10000000000.0) should be (100000.0 +- TestTolerance)
  }

  it should "work well for big odd results" in {
    Sqrt.sqrt(123123123123.0) should be (350889.0467 +- TestTolerance)
  }
}
