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

/*
  Exercise 5.2.3 Write factorial in terms of product .
*/

/*
  Exercise 5.2.4 Can you write an even more general function which generalizes both
  sum and product ?
*/


