package recursion

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def countParenthesis(chars: List[Char], openCount: Int): Boolean = chars match {
      case Nil => isZero(openCount)
      case '(' :: cs => countParenthesis(cs, openCount + 1)
      case ')' :: cs => if (isZero(openCount)) false else countParenthesis(cs, openCount - 1)
      case _ :: cs => countParenthesis(cs, openCount)
    }
    def isZero(number: Int): Boolean = number == 0

    countParenthesis(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (coins.isEmpty) 0
    else if (coins.head <= money) countChange(money - coins.head, coins) + countChange(money, coins.tail)
    else countChange(money, coins.tail)
  }
}
