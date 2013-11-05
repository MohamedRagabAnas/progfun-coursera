package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("2 elem, minimum are min of the two") =
    forAll { (a: Int, b: Int) =>
      val h = insert(a, insert(b, empty))
      findMin(h) == Math.min(a, b)
    }

  property("delete min from 1 elem results empty") =
    forAll { a: Int =>
      val h1 = insert(a, empty)
      deleteMin(h1) == empty
    }

  property("min meld is min of one or another") =
    forAll { (h1: H, h2: H) =>
      val hm = meld(h1, h2)
      findMin(hm) == Math.min(findMin(h1), findMin(h2))
    }

  property("Sorted sequence when deleting min") =
    forAll { (h: H) =>
      val seq = elemSeq(h, Seq())
      seq == seq.sorted
    }

  def elemSeq(h: H, s: Seq[A]): Seq[A] = {
    if (isEmpty(h)) s
    else elemSeq(deleteMin(h), s ++ Seq(findMin(h)))
  }

  lazy val genHeap: Gen[H] = for {
    e <- arbitrary[A]
    h <- oneOf(empty, genHeap)
  } yield insert(e, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

}
