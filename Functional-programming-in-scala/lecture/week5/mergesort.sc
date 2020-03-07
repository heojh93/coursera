package week5

// Lecture 5.2 -- Pairs and Tuples
// Lecture 5.3 -- Parameterizing merge sort function
//
// merge sort is more efficient than insertion sort
// If a list constists of zero or one elements, it is already sorted.
// Otherwise, split the list in two, sort the parts, then merge them.
// Here's a first attempt at merge sort:
object mergesort {

	def msort[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
		val n = xs.length/2
		if (n == 0) xs
		else {
			def merge(xs: List[T], ys: List[T]): List[T] =
			(xs, ys) match {
				case (Nil, ys) => ys
				case (xs, Nil) => xs
				case (x :: xst, y :: yst) =>
					if (ord.lt(x, y)) x :: merge(xst, ys)
					else y :: merge(xs, yst)
			}
			val (first, second) = xs splitAt n
			merge(msort(first), msort(second))
		}
	}                                         //> msort: [T](xs: List[T])(implicit ord: Ordering[T])List[T]

	val x = List(5, 2, -4, 9, 7)              //> x  : List[Int] = List(5, 2, -4, 9, 7)
	val fruits = List("apple", "pineapple", "orange", "banana")
                                                  //> fruits  : List[String] = List(apple, pineapple, orange, banana)
	
	msort(x)                                  //> res0: List[Int] = List(-4, 2, 5, 7, 9)
	msort(fruits)                             //> res1: List[String] = List(apple, banana, orange, pineapple)
}