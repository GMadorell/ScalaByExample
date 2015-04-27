
/*
  Exercise 5.2.1 1. The sum function uses a linear recursion. Can you write a tail-
  recursive one by filling in the ??’s?
    def sum(f: Int => Int)(a: Int, b: Int): Int = {
      def iter(a: Int, result: Int): Int = {
        if (??) ??
        else iter(??, ??)
      }
      iter(??, ??)
    }
*/
def sum(f: Int => Int)(a: Int, b: Int): Int = {
  def iter(a: Int, accumulator: Int): Int = {
    if (a > b) accumulator
    else iter(a + 1, accumulator + f(a))
  }
  iter(a, 0)
}

assert(sum(x => x)(1, 3) == 6)
assert(sum(x => x)(3, 6) == 18)
assert(sum(x => x * x)(1, 2) == 5)
assert(sum(x => x * x)(3, 4) == 25)
/*
  Exercise 5.2.2 Write a function product that computes the product of the values of
  functions at points over a given range.
*/
def product(f: Int => Int)(minRange: Int, maxRange: Int): Int = {
  def iter(counter: Int, accumulator: Int): Int = {
    if (counter > maxRange) accumulator
    else iter(counter + 1, accumulator * f(counter))
  }
  iter(minRange, 1)
}

assert(product(x => x)(0, 10000000) == 0)
assert(product(x => x)(1, 3) == 1 * 2 * 3)
assert(product(x => x)(3, 6) == 3 * 4 * 5 * 6)
assert(product(x => x * x)(1, 2) == 1 * 1 * 2 * 2)
assert(product(x => x * x)(3, 4) == 3 * 3 * 4 * 4)
/*
  Exercise 5.2.3 Write factorial in terms of product .
*/
def factorial(n: Integer): Integer = product(x => x)(1, n)
assert(factorial(1) == 1)
assert(factorial(2) == 2)
assert(factorial(3) == 6)
assert(factorial(4) == 24)
assert(factorial(10) == 3628800)
/*
  Exercise 5.2.4 Can you write an even more general function which generalizes both
  sum and product ?
*/
def iterate(f: Int => Int)
           (minRange: Int, maxRange: Int)
           (startValue: Integer, accumulateFunction: (Int, Int) => Int): Integer = {
  def iter(counter: Int, accumulator: Int): Int = {
    if (counter > maxRange) accumulator
    else iter(counter + 1, accumulateFunction(accumulator, f(counter)))
  }
  iter(minRange, startValue)
}

def sumGeneralized(f: Int => Int)(minRange: Int, maxRange: Int) =
  iterate(f)(minRange, maxRange)(0, (accumulator, calculatedValue) => accumulator + calculatedValue)

def productGeneralized(f: Int => Int)(minRange: Int, maxRange: Int) =
  iterate(f)(minRange, maxRange)(1, (accumulator, calculatedValue) => accumulator * calculatedValue)


assert(sumGeneralized(x => x)(1, 3) == 6)
assert(sumGeneralized(x => x)(3, 6) == 18)
assert(sumGeneralized(x => x * x)(1, 2) == 5)
assert(sumGeneralized(x => x * x)(3, 4) == 25)
assert(productGeneralized(x => x)(0, 10000000) == 0)
assert(productGeneralized(x => x)(1, 3) == 1 * 2 * 3)
assert(productGeneralized(x => x)(3, 6) == 3 * 4 * 5 * 6)
assert(productGeneralized(x => x * x)(1, 2) == 1 * 1 * 2 * 2)
assert(productGeneralized(x => x * x)(3, 4) == 3 * 3 * 4 * 4)

/*
  Exercise 5.3.1 Write a function for cube roots using fixedPoint and averageDamp .
*/
val tolerance = 0.0001
def isCloseEnough(x: Double, y: Double) = math.abs((x - y) / x) < tolerance
def fixedPoint(f: Double => Double)(firstGuess: Double) = {
  def iterate(guess: Double): Double = {
    val next = f(guess)
    if (isCloseEnough(guess, next)) next
    else iterate(next)
  }
  iterate(firstGuess)
}

def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2
def sqrt(x: Double) = fixedPoint(averageDamp(y => x/y))(1.0)
def cubeRoot(x: Double) = fixedPoint(averageDamp(y => x / (y*y)))(1.0)
assert(isCloseEnough(cubeRoot(3.0), 1.44225))
assert(isCloseEnough(cubeRoot(8.0), 2.0))

