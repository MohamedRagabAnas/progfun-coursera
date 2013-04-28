object intsets {
  val t1 = new NonEmpty(1, Empty, Empty)          //> t1  : NonEmpty = {.1.}
  val t2 = t1.incl(4)                             //> t2  : IntSet = {.1{.4.}}

  val t3 = new NonEmpty(5, Empty, Empty)          //> t3  : NonEmpty = {.5.}

  t3 union t2                                     //> res0: IntSet = {.1{.4{.5.}}}
//  new IntSet
}

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet): IntSet
}

object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  def union(other: IntSet) = other
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }
  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    if (x > elem) new NonEmpty(elem, left, right  incl x)
    else this
  }
  
  def union(other: IntSet): IntSet =
    ((left union right) union other) incl elem
  
  override def toString = "{" + left + elem + right + "}"
}