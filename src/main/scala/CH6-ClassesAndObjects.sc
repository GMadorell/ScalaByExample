


/*
  Exercise 6.0.1 Write methods union and intersection to form the union and in-
    tersection between two sets.

  Exercise 6.0.2 Add a method
      def excl(x: Int)
    to return the given set without the element x . To accomplish this, it is useful to also
    implement a test method
      def isEmpty: Boolean
    for sets.
*/


trait IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
  def intersection(other: IntSet): IntSet
  def excl(x: Int): IntSet
  def isEmpty: Boolean
}

class EmptySet extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
  def union(other: IntSet): IntSet = other
  def intersection(other: IntSet): IntSet = new EmptySet()
  def excl(x: Int): IntSet = new EmptySet()
  def isEmpty = true
}

class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this

  def union(other: IntSet): IntSet = {
    new NonEmptySet(elem, left union other, right union other)
  }

  def intersection(other: IntSet): IntSet = {
    val s = (left intersection other) union (right intersection other)
    if (other contains elem) s incl elem else s
  }

  def excl(x: Int): IntSet = {
    if (x < elem) new NonEmptySet(elem, left excl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right excl x)
    else left union right
  }

  def isEmpty = false
}

def createSetIncludingAll(elements: List[Int]): IntSet = {
  var set: IntSet = new EmptySet
  elements.foreach(element => set = set.incl(element))
  set
}

def doesSetContainsAll(set: IntSet, elements: List[Int]): Boolean = {
  var containsAll = true
  elements.foreach(element => if (!set.contains(element)) containsAll = false)
  containsAll
}

val testSet = createSetIncludingAll(List(1, 2, 3))
val testSet2 = createSetIncludingAll(List(1, 2, 3, 4))
val testUnion = testSet union testSet2
val testIntersection = testSet intersection testSet2
assert(doesSetContainsAll(testUnion, List(1, 2, 3, 4)))
assert(doesSetContainsAll(testIntersection, List(1, 2, 3)))
assert(doesSetContainsAll(testSet excl 1, List(2, 3)))
assert(doesSetContainsAll(testSet excl 1 excl 2, List(3)))
assert(doesSetContainsAll(testSet excl 1 excl 2 excl 3, List()))

