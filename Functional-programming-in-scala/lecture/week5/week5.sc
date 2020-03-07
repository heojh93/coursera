package week5

object week5 {
	val fruit = List("apples", "oranges", "pears")
                                                  //> fruit  : List[String] = List(apples, oranges, pears)
	val nums = List(1,2,3)                    //> nums  : List[Int] = List(1, 2, 3)
	val diag3 = List(List(1,0,0), List(0,1,0), List(0,0,1))
                                                  //> diag3  : List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
                                                  //| 


	val empty = List()                        //> empty  : List[Nothing] = List()

	// This answer (given in the video) is incorrect:
	// def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n+1)

	// Here is the correct answer:
	def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)
                                                  //> removeAt: [T](n: Int, xs: List[T])List[T]

	
	removeAt(1, List('a', 'b', 'c', 'd'))     //> res0: List[Char] = List(a, c, d)
}