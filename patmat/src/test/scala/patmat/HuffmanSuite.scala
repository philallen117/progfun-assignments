package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("times") {
    val h = string2Chars("hello, world")
    val ht = times(h)
    assert(ht.contains(('l', 3)))
    assert(ht.contains(('w', 1)))
    assert(ht.length === 9)
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton") {
    new TestTrees {
      assert(!singleton(Nil))
      assert(singleton(List(t1)))
      assert(!singleton(List(t1, t2)))
    }
  }

  test("cct 1") {
    val t = createCodeTree("some text".toList)
    assert(true)
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
      assert(decode(t2, encode(t2)("adb".toList)) === "adb".toList)
    }
  }

  test("quick encode and decode a very short text should be identity") {
    new TestTrees {
      val abCoded = quickEncode(t1)("ab".toList)
      val adbCoded = quickEncode(t2)("adb".toList)
      assert(decode(t1, abCoded) === "ab".toList)
      assert(decode(t2, adbCoded) === "adb".toList)
    }
  }

  test("french quick encode decode") {
    val trySecret = quickEncode(frenchCode)("huffmanestcool".toList)
    assert(trySecret === secret)
  }

}
