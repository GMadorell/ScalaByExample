
object Sqrt {
  def sqrt(n: Double): Double = {
    def sqrtIterate(guess: Double, n: Double): Double = {
      if (isGoodEnough(guess, n)) guess
      else sqrtIterate(improve(guess, n), n)
    }
    def isGoodEnough(guess: Double, n: Double): Boolean = {
      math.abs(square(guess) - n) < 0.001
    }
    def square(n: Double): Double = math.pow(n, 2)
    def improve(guess: Double, n: Double): Double = {
      (guess + n / guess) / 2
    }
    sqrtIterate(1, n)
  }
}

