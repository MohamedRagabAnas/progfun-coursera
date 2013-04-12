package recfun
import common._

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
  def pascal(c: Int, r: Int): Int =
    if (c > r || c < 0 || r < 0) throw new IllegalArgumentException("invalid row or col")
    else if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    chars.foldLeft(0) { (sum, current) =>
      if (sum < 0) -1
      else if (current == '(') sum + 1
      else if (current == ')') sum - 1
      else sum
    } == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    countChangeSorted(money, coins.sortWith((a, b) => a < b))
  }

  private def countChangeSorted(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 0
    else if (coins.head == money) 1
    else if (coins.head > money) countChange(money, coins.tail)
    else if (coins.tail.isEmpty && (money % coins.head == 0)) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }

}
