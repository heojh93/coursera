object exercise1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  // This is a tail recursive version of the standard factorial function.
	// This is mainly an academic exercise, since the factorial grows so quickly that
	// the standard, non-tail recursive version encounters integer overflow well before
	// stack frame overflow.
	def factorial(x: Int) = {

		def factorialTailRecursive(y: Int, acc: Int): Int = {
			if (y==0) acc else factorialTailRecursive(y-1, y*acc)
		}

		if (x<0) {
			-1
		} else if (x==0) {
			1
		} else {
			factorialTailRecursive(x,1)
		}
	}                                         //> factorial: (x: Int)Int

	factorial(-2)                             //> res0: Int = -1
	factorial(-1)                             //> res1: Int = -1
	factorial(0)                              //> res2: Int = 1
	factorial(1)                              //> res3: Int = 1
	factorial(2)                              //> res4: Int = 2
	factorial(3)                              //> res5: Int = 6
	factorial(4)                              //> res6: Int = 24

}

	// Lecture 2.1:  Higher Order Functions