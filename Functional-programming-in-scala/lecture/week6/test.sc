package week6

object test {

	// Lecture 6.1 -- sequence operations can be applied to all sequence like collections.
	val xs = Array(1, 2, 3, 44)               //> xs  : Array[Int] = Array(1, 2, 3, 44)
	xs map (x => 2*x)                         //> res0: Array[Int] = Array(2, 4, 6, 88)
	
	val s = "Hello, World!"                   //> s  : String = Hello, World!
	s filter (x => x.isUpper)                 //> res1: String = HW
	s filter (x => x.isLower)                 //> res2: String = elloorld
	
	// Range is another subclass of Iterable
	val r: Range = 1 until 5                  //> r  : Range = Range(1, 2, 3, 4)
	val t: Range = 1 to 5                     //> t  : Range = Range(1, 2, 3, 4, 5)
	1 to 10 by 3                              //> res3: scala.collection.immutable.Range = Range(1, 4, 7, 10)
	6 to 1 by -2                              //> res4: scala.collection.immutable.Range = Range(6, 4, 2)
	
	// More sequence operations
	
	// exists
	s exists (c => c.isUpper)                 //> res5: Boolean = true
	// forall
	s forall (c => c.isUpper)                 //> res6: Boolean = false
	
	// zip and unzip
	val pairs = List(1, 2, 3) zip s           //> pairs  : List[(Int, Char)] = List((1,H), (2,e), (3,l))
	pairs unzip                               //> res7: (List[Int], List[Char]) = (List(1, 2, 3),List(H, e, l))
	
	// flatMap (apply a map that takes an element to a singleton list)
	s flatMap (c => List(c.toUpper))          //> res8: String = HELLO, WORLD!
	
	// but the above is probably more naturally written as
	s map (c => c.toUpper)                    //> res9: String = HELLO, WORLD!
	
	// sum
	xs.sum                                    //> res10: Int = 50
	// max
	xs.max                                    //> res11: Int = 44
	// min
	xs.min                                    //> res12: Int = 1
	
  // continued in worksheet combinations.sc
	
}