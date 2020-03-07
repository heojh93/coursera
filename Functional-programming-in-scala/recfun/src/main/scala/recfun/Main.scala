package recfun

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
      if(r<0 || c<0) -1
      else if(c == 0 | r == 0 || r == 1) 1
      else pascal(c-1,r-1) + pascal(c, r-1)
    }
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checker(diff: Int, chars: List[Char]): Boolean = {
        if (diff < 0) false
        else if (chars.isEmpty){
          if(diff == 0) true
          else false
        }
        else{
          if (chars.head == '(') {true && checker(diff+1, chars.tail)}
          else if( chars.head == ')') {true && checker(diff-1, chars.tail)}
          else {true && checker(diff, chars.tail)}
        }
      }

      checker(0, chars)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money > 0 && !coins.isEmpty)
        countChange(money-coins.head, coins) + countChange(money, coins.tail)
      else
        0
    }
  }
