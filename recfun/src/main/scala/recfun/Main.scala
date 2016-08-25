package recfun

import scala.annotation.tailrec
import scala.beans.BooleanBeanProperty

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(c: Int, r: Int): Int = {
    // 0 <= c <= r - c, 0 <= r
    def aux(c: Int, r: Int): Int =
      if (r == 0) 1
      else if (c == 0) 1
      else if (c == r) 1
      else aux(c - 1, r - 1) + aux(c, r - 1) // 0 < r, 0 < c < r

    if (c < 0) throw new IllegalArgumentException("Column must be non-negative")
    if (r < 0) throw new IllegalArgumentException("Row must be non-negative")
    if (c > r) throw new IllegalArgumentException("Column undefined for that row")
    aux(c, r)
  }

  /**
    * Exercise 2
    */

  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def aux(depth: Int, chars: List[Char]): Boolean =
      if (depth >= 0) {
        if (chars.isEmpty) depth == 0
        else {
          val h = chars.head
          if (h == '(') aux(depth + 1, chars.tail)
          else if (h == ')') aux(depth - 1, chars.tail)
          else aux(depth, chars.tail)
        }
      }
      else false

    aux(0, chars)
  }
  
  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    def count(balance: Int, sortedCoins: List[Int]): Int =
      if (sortedCoins.isEmpty) 0
      else
        count(balance, sortedCoins.tail) + {
          val newBalance = balance - sortedCoins.head
          if (newBalance <  0) 0
          else if (newBalance == 0) 1
          else count(newBalance, sortedCoins)
        }

    count(money, coins.sorted(Ordering[Int].reverse))
  }

}
