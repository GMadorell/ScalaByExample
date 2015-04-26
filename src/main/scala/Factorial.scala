

object Factorial {
  def tailRecursive(n: Integer): Integer = {
    def factorialIter(n: Integer, accumulator: Integer): Integer = {
      if (n <= 1) accumulator else factorialIter(n - 1, accumulator * n)
    }
    factorialIter(n, 1)
  }

  def headRecursive(n: Integer): Integer = {
    if (n == 0) 1 else n * Factorial.headRecursive(n - 1)
  }
}

