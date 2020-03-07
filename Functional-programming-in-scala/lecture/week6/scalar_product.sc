package week6


// Lecture 6.1
object scalar_product1 {
	
	def scalarProduct( xs: Vector[Double], ys: Vector[Double]): Double =
		(xs zip ys).map(xy => xy._1 * xy._2).sum
                                                  //> scalarProduct: (xs: Vector[Double], ys: Vector[Double])Double
	
	val xs = Vector(1.0, 1.0, 1.0); val ys = Vector(1.0, 2.0, 3.0)
                                                  //> xs  : scala.collection.immutable.Vector[Double] = Vector(1.0, 1.0, 1.0)
                                                  //| ys  : scala.collection.immutable.Vector[Double] = Vector(1.0, 2.0, 3.0)

	scalarProduct(xs, ys)                     //> res0: Double = 6.0
	
	// Exercise: isPrime (19'30")
	// First pass at a function to test if an integer is prime:
	def isPrime1(n: Int): Boolean =
		if (n<2) false
		else if ((2 until n) flatMap (x => (2 until n) map (y => (x,y))) exists {case (x,y) => (x*y==n)}) false
		else true                         //> isPrime1: (n: Int)Boolean

	// The above works, but here is a much more concise version (which only works for n>1):
	def isPrime(n: Int): Boolean = (2 until n) forall (k => (n%k != 0))
                                                  //> isPrime: (n: Int)Boolean

	isPrime(1)                                //> res1: Boolean = true
	isPrime(2)                                //> res2: Boolean = true
	isPrime(3)                                //> res3: Boolean = true
	isPrime(4)                                //> res4: Boolean = false
	isPrime(5)                                //> res5: Boolean = true
	isPrime(6)                                //> res6: Boolean = false
	(2 until 5) flatMap (x => (2 until 5) map (y => (x,y)))
                                                  //> res7: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((2,2), (2,3
                                                  //| ), (2,4), (3,2), (3,3), (3,4), (4,2), (4,3), (4,4))
	
}


object scalar_product2 {

	def scalarProduct(xs: Vector[Double], ys: Vector[Double]): Double =
		(xs zip ys) map { case (x,y) => x*y } sum
		// This could also have been written as follows:
		//(xs zip ys).map{ case (x,y) => x*y }.sum
		
		// Note that
		//
		//            { case p1 => e1 ... case pn => en }
		//
		// is equivalent to
		//
		//            x => x match { case p1 => e1 ... case pn => en }
		//
		// but the former is more concise and clearer.
		
	val xs = Vector(1.0, 1.0, 1.0); val ys = Vector(1.0, 2.0, 3.0)
	
	scalarProduct(xs, ys)
		
}