package week6

object convolution {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  	//def cyclicShift(xs: Vector[Double], n: Int): Vector[Double] =
	def cyclicConvolution( xs: Vector[Double], ys: Vector[Double]): (Int => Double) =
		(0 until xs.length) map (xs zip ys).map(xy => xy._1 * xy._2)
                                                  //> cyclicConvolution: (xs: Vector[Double], ys: Vector[Double])Int => Double

	val xs = Vector(1.0, 1.0, 1.0); val ys = Vector(1.0, 2.0, 3.0)
                                                  //> xs  : scala.collection.immutable.Vector[Double] = Vector(1.0, 1.0, 1.0)
                                                  //| ys  : scala.collection.immutable.Vector[Double] = Vector(1.0, 2.0, 3.0)
	cyclicConvolution(xs, ys)                 //> res0: Int => Double = Vector(1.0, 2.0, 3.0)
  
}