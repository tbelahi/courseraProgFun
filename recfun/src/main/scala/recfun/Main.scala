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
    println(countChange(4, List(1,2)))  
  }

  /**
   * Exercise 1
   * recursive solution to pascal's triangle
   */
  def pascal(c: Int, r: Int): Int = 
    /* termination condition: pascal(c,r) = 1 everywhere on first 
     * column or on the diagonal (triangle is organised in a  lower triangular
     * matrix), i.e c=0 or c=r
     * recursive call : p(c,r) = p(c-1,r-1) + p(c,r-1)
     */
    if (c==0 || r==c) 1 else pascal(c-1,r-1) + pascal(c, r-1)

  /**
   * Exercise 2: Parantheses balancing
   * need to loop over the string and count opening 
   * and closing parantheses and so some king of mass balance.
   * Parantheses are balanced if the balance is 0 at the end.
   * If unbalanced parantheses, at some point the balance will
   * be negative or at the end the balance is > 0, this two cases
   * leads to failure.
   * looping over the list with recursion implies to use a tail recursion.
   * 
   */

  def balance(chars: List[Char]): Boolean = {
    
    def inerBalance(openedparan: Int, chars: List[Char]): Boolean=
      /* termination conditions, empty string no parantheses open or to close*/
      if (chars.isEmpty && openedparan==0) true 
      else if (chars.isEmpty && openedparan!=0) false
      /*recursive call*/
      else {
        val head = chars.head
        val rest = chars.tail
        val n = 
          if (head=='(') openedparan + 1
          else if (head==')') openedparan - 1
          else openedparan
        if (n>=0) inerBalance(n, rest)
        else false /* terminates if openedparan, e.g. n, is negative*/
      }

      inerBalance(0, chars)
  }

  /**
   * Exercise 3: find the number of ways to give change back
   * given an amount to give back and a list of the coins available
   * Again, tail recursion is needed to iterate over the list of coins
   * tree recursion solves the problem here
   * see : http://mitpress.mit.edu/sicp/full-text/book/book-Z-H-11.html#%_idx_722
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    /* check that there are coins to give change or if change should be given back*/
    if (coins.isEmpty || money<=0) 0
    /* recursively count the ways to give change back*/
    else {
      def countWays(money: Int, coins: List[Int]): Int = {
        /* termination conditions */
        if (coins.isEmpty || money < 0) 0
        /*another termination condition*/
        else if (money == 0) 1
        /* recursive call, it is a tree recursion as countWays is called twice with different
         * arguments, thus it leads to branching
         */
        else {
          val head = coins.head
          val rest = coins.tail
          countWays(money-head, coins) + countWays(money, rest)
      }
    }
    countWays(money, coins)
    }
      
  }
}
