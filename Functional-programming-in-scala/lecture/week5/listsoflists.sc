package week5

// Practice with lists, and some useful list functions.

object listoflists {

	// Max of a list of Ints
 	// def maxn (xs: List[Int]) = (xs reduceLeft)(_ max _)
 	// Removing this because the same is accomplished with .max
 	// maxn(List(2, -4, 5))
 	// maxn(List(2))
 	List(2, -4, 5).max                        //> res0: Int = 5
 	List(2, -4, 5).min                        //> res1: Int = -4
 	List(2).max                               //> res2: Int = 2
 	 	
	// Take a list of lists and return the length of the longest one.
	// def maxLen[T](xs: List[List[T]]) = (xs map (x=>x.length)) reduceLeft (_ max _)
	def maxLen[T](xs: List[List[T]]) = (xs map (x=>x.length)).max
                                                  //> maxLen: [T](xs: List[List[T]])Int

	// Take a list of lists and return the index of the shortest one
	def minLenIdx[T](xs: List[List[T]]) = xs.zipWithIndex.minBy(_._1.length)._2
                                                  //> minLenIdx: [T](xs: List[List[T]])Int

	// Take a list of lists and return the length of the shortest one
	def minLen[T](xs: List[List[T]]) = (xs map(x => x.length)).min
                                                  //> minLen: [T](xs: List[List[T]])Int

	// Tests:
  val myPlist = List(List("a", "a", "a"), List("b"), List("c", "c"))
                                                  //> myPlist  : List[List[String]] = List(List(a, a, a), List(b), List(c, c))
	maxLen(myPlist)                           //> res3: Int = 3
	minLen(myPlist)                           //> res4: Int = 1
	minLenIdx(myPlist)                        //> res5: Int = 1


	// append a to the n-th list in a list of lists
	def pushAt[T](xs: List[List[T]], a: T, n: Int): List[List[T]] = {
		if (n==0) (xs.head ::: List(a)) :: xs.tail else xs.head :: pushAt(xs.tail, a, n-1)
	}                                         //> pushAt: [T](xs: List[List[T]], a: T, n: Int)List[List[T]]
	
	// concatenate two lists (complexity: n)
	def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
		case List() => ys
		case z :: zs => z :: concat(zs, ys)
	}                                         //> concat: [T](xs: List[T], ys: List[T])List[T]



	def pushAtShortest[T](xs: List[List[T]], a: T): List[List[T]] =	pushAt(xs, a, minLenIdx(xs))
                                                  //> pushAtShortest: [T](xs: List[List[T]], a: T)List[List[T]]
	
                                                  
	// Add a new row when the number of rows is less than the length of the shortest list
	// Add a new row with a single entry t
	def newRow[T](xs: List[List[T]], a: T): List[List[T]] = {
		if (xs == Nil) List(List(a)) else xs ::: List(List(a))
	}                                         //> newRow: [T](xs: List[List[T]], a: T)List[List[T]]
	
  // TESTS
	val mynewlist = List(List("a", "a", "a"), List("c", "c", "c"), List("q", "q", "q"))
                                                  //> mynewlist  : List[List[String]] = List(List(a, a, a), List(c, c, c), List(q
                                                  //| , q, q))
  minLen(mynewlist)                               //> res6: Int = 3
  minLenIdx(mynewlist)                            //> res7: Int = 0
	pushAt(mynewlist, "d", 2)                 //> res8: List[List[String]] = List(List(a, a, a), List(c, c, c), List(q, q, q,
                                                  //|  d))
	pushAtShortest(mynewlist, "d")            //> res9: List[List[String]] = List(List(a, a, a, d), List(c, c, c), List(q, q,
                                                  //|  q))

	newRow(mynewlist, "f")                    //> res10: List[List[String]] = List(List(a, a, a), List(c, c, c), List(q, q, q
                                                  //| ), List(f))
	newRow(pushAtShortest(mynewlist, "d"), "f")
                                                  //> res11: List[List[String]] = List(List(a, a, a, d), List(c, c, c), List(q, q
                                                  //| , q), List(f))

	pushAtShortest(newRow(mynewlist, "f"), "d")
                                                  //> res12: List[List[String]] = List(List(a, a, a), List(c, c, c), List(q, q, q
                                                  //| ), List(f, d))

	                                                   
}