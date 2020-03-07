import math.abs

// Lecture 2.3: Finding Fixed Points
object exercise4 {
	val tolerance = 0.0001                    //> tolerance  : Double = 1.0E-4
	def isCloseEnough(x: Double, y: Double) =
		abs((x - y) / x) < tolerance      //> isCloseEnough: (x: Double, y: Double)Boolean
	def fixedPoint(f: Double => Double)(firstGuess: Double) = {
		def iterate(guess: Double): Double = {
			val next = f(guess)
			if (isCloseEnough(guess, next)) next
			else iterate(next)
			}
			iterate(firstGuess)

		}                                 //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
		fixedPoint(x => 1 + x/2)(1)       //> res0: Double = 1.9998779296875
}