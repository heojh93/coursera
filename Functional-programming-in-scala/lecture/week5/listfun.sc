package week5

// Lecture 4.7
// Insertion sort exercise:
object insertionsort {

	def isort(xs: List[Int]): List[Int] = xs match {
		case List() => List()
		case y :: ys => insert(y, isort(ys))
	}                                         //> isort: (xs: List[Int])List[Int]

	def insert(x: Int, xs: List[Int]): List[Int] = xs match {
		case List() => List(x)
		case y :: ys => if (x <= y) x :: xs else y :: insert(x, ys)
	}                                         //> insert: (x: Int, xs: List[Int])List[Int]
}

// Lecture 5.1: More Functions on Lists
object initandconcatfunctions {

	// exercise: init function
	def init[T](xs: List[T]): List[T] = xs match {
		case List() => throw new Error("init on empty list")
		case List(x) => List()
		case y :: ys => y :: init(ys)
	}
	
	// concat function (complexity: n)
	def concat[T](xs: List[T], ys: List[T]): List[T] = xs match {
		case List() => ys
		case z :: zs => z :: concat(zs, ys)
	}
	
	// reverse function  (complexity: n^2)
	def reverse[T](xs: List[T]): List[T] = xs match {
		case List() => xs
		case y :: ys => reverse(ys) ++ List(y)
	}
	
	// exercise: remove nth element
	// My first attempt (I believe this works, but below there's a better version):
	//def removeAt[T](n: Int, xs: List[T]): List[T] = xs match {
	//		case List() => throw new Error("removeAt called on empty list")
			//case y :: ys => if (n == 0) ys else y :: removeAt(n-1, ys)
		//}
	 
	// The answer given in the video (which is incorrect):
	// def removeAt(n: Int, xs: List[Int]) = (xs take n) ::: (xs drop n+1)

	// My second attempt (this works---see week5.sc worksheet for tests)
	def removeAt[T](n: Int, xs: List[T]) = (xs take n) ::: (xs drop n+1)

}

// Lecture 5.4: Higher Order List Functions (application: run-length encoding)
// Lecture 5.5: Reduction of Lists
//
object listfun {
	val nums = List(2, -4, 5, 7, 1)
	val fruits = List("apple", "pineapple", "orange", "banana")

	nums filter (x => x > 0)
	nums filterNot (x => x > 0)
	nums partition (x => x > 0)

	// Longest prefix of the list for which predicate is true
	nums takeWhile (x => x > 0)
	nums dropWhile (x => x > 0)
	nums span (x => x > 0)
	
	val data = List("a", "a", "a", "b", "c", "c", "a")
	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
		case x :: xt =>
			val (first, rest) = xs span (y => y == x)
			first :: pack(rest)
	}
	pack(data)
	def encode[T](xs: List[T]): List[(T, Int)] =
		pack(xs) map (ys => (ys.head, ys.length))
		
	encode(data)
	
	def sum(xs: List[Int]) = (0 :: xs) reduceLeft ((x, y) => x + y)
	def product(xs: List[Int]) = (1 :: xs) reduceLeft ((x, y) => x * y)
	// The n-th underscore is shorthand for the n-th parameter
	def sum2(xs: List[Int]) = (0 :: xs) reduceLeft (_ + _)
	def product2(xs: List[Int]) = (1 :: xs) reduceLeft (_ * _)
	def sum3(xs: List[Int]) = (xs foldLeft 0) (_ + _)
	def product3(xs: List[Int]) = (xs foldLeft 1) (_ * _)

	def concat[T](xs: List[T], ys: List[T]): List[T] = (xs foldRight ys)(_ :: _)

	sum(nums)
	sum2(nums)
	sum3(nums)
	product(nums)
	product2(nums)
	product3(nums)
	
	concat(fruits, data)
                                                  
}

// Review of Week 5:
object listfun2 {

  val nums = List(2, -4, 5, 7, 1)
  val fruits = List("apple", "pineapple", "orange", "banana")
  
  nums filter (x => x>0)
  nums filterNot (x => x > 0)
  // do both in a single traversal
  nums partition (x => x > 0)
  
  nums takeWhile (x => x>0)
  nums dropWhile (x => x>0)
  // do both in a single traversal
  nums span (x => x>0)
  
  // Exercise: Lecture 5.4 at 10'45"
  //
  // Write a function that packs lists like this one:
  //    List("a", "a", "a", "b", "c", "c", "a")
  // and returns the list
  //    List(List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
  //
 	def pack[T](xs: List[T]): List[List[T]] = xs match {
		case Nil => Nil
 	 	case y :: ys =>
 	 		val (xs1, xs2) = xs span (x => (x==y))
 	 		xs1 :: pack(xs2)
 	}
 	 
 	 // Test it:
 	 val mylist = List("a", "a", "a", "b", "c", "c", "c", "c", "a")
 	 
 	 val myPlist = pack(mylist)
  // Exercise: Lecture 5.4 at 12'45"
  //
  // Write a function that produces a runlength encoding of a list:
  // Input:
  //    List("a", "a", "a", "b", "c", "c", "a")
  // Output:
  //    List(("a", 3), ("b", 1), ("c", 2), ("a", 1))
  //
 	def encode[T](xs: List[T]): List[(T, Int)] = xs match {
 		case Nil => Nil
 		case y :: ys => {
 			val (xs1, xs2) = xs span (x => (x==y))
 			(y, xs1.length) :: encode(xs2)
 		}
 		
 	}
 	
 	encode(mylist)



}

object reduction_of_lists {

// Lecture 5.5: Exercise concat with foldRight (12'45")
	def concat[T](xs: List[T], ys: List[T]): List[T] =
		(xs foldRight ys) (_ :: _)
}






   