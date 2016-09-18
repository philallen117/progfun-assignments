package quickcheck

import common._
import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  def heapEqual(h1: H, h2: H): Boolean =
    if (isEmpty(h1) && isEmpty(h2)) true
    else {
      val m1 = findMin(h1)
      val m2 = findMin(h2)
      m1 == m2 && heapEqual(deleteMin(h1), deleteMin(h2))
    }

  val e = empty

  //  If you insert any two elements into an empty heap,
  //  finding the minimum of the resulting heap should get the smallest of the two elements back.
  property("min2e") = 1 ?= findMin(insert(2, insert(1, e)))

  //  If you insert an element into an empty heap,
  //  then delete the minimum,
  //  the resulting heap should be empty.
  property("insertDelete") = {
    isEmpty(deleteMin(insert(-1, e)))
  }

  property("equality1") = {
    heapEqual(insert(1, insert(2, e)), insert(2, insert(1, e)))
  }

  lazy val genHeap: Gen[H] = for {
    n <- arbitrary[A]
    h <- frequency((1, Gen.const(empty)), (4, genHeap))
  } yield insert(n, h)
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("insertMinPreservesMin1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("deleteMinRemovesLeast1") = forAll { (h: H, a: A) =>
    isEmpty(h) | {
      val a1 = findMin(h)
      a1 <= a | findMin(insert(a, h)) == a
    }
  }

  //  Given any heap, you should get a sorted sequence of elements when continually finding and deleting minima
  //    (Hint: recursion and helper functions are your friends.)
  property("sorted") = forAll { (h: H) =>
    def aux(acc: List[A], h: H): List[A] =
      if (isEmpty(h)) acc
      else aux (findMin(h) :: acc, deleteMin(h))
    def getAll(h: H): List[A] = aux(List(), h)
    val l = getAll(h)
    l.sorted == l.reverse
  }

  //  Finding a minimum of the melding of any two heaps should return a minimum of one or the other.
  property("meld") = forAll { (h1: H, h2: H) =>
    val m1 = findMin(h1)
    val m2 = findMin(h2)
    val m = m1 min m2
    findMin(meld(h1, h2)) == m
  }

  // Base condition on meld
  property("meldInductive") = forAll { (h1: H) =>
    heapEqual(meld(e, h1), h1)
  }

  // Inductive condition on meld
  property("meldInductive") = forAll { (h1: H, h2: H) =>
    heapEqual(meld(h1, h2),
      meld(deleteMin(h1), insert(findMin(h1), h2)))
  }

}
