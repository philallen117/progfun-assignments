import patmat.Huffman._

val transSecret = decode(frenchCode, secret)

def findNext(tree: CodeTree, bits: List[Bit]): (Char, List[Bit]) = tree match {
  case Leaf(c, _) => (c, bits)
  case Fork(l, r, c, _) =>
    if (bits.head == 0) findNext(l, bits.tail)
    else findNext(r, bits.tail)
}

val (c1, r1) = findNext(frenchCode, secret)
val (c2, r2) = findNext(frenchCode, r1)

def printBits(l: List[Bit]) = (l map (b => if(b==0) "0" else "1"))

val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
combine(leaflist)

def insertInt(i: Int, li: List[Int]) = {
  def aux(acc: List[Int], li: List[Int]): List[Int] = li match {
    case Nil => acc ::: List(i)
    case j::rest =>
      if (i <= j) acc ::: (i :: li)
      else aux(acc ::: List(j), rest)
  }
  aux(Nil, li)
}

val sortedList = makeOrderedLeafList(times("some text".toList))
sortedList.length
val single: List[CodeTree] = until(singleton, combine)(sortedList)
single.length


val t = createCodeTree("some text".toList)
val bl = encode(t)("some text".toList)

print("", frenchCode)
print("", t)