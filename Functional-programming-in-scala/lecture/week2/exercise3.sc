// Lecture 2.2: Conclusion

object exercise3 {
	// As an exercise, we will do the same as we did for sums, but now with product.
  def product(f: Int => Int)(a: Int, b: Int): Int = {
 		if (a > b) 1 else f(a) * product(f)(a + 1, b)
 	}                                         //> product: (f: Int => Int)(a: Int, b: Int)Int
 	product(x => x * x)(3, 4)                 //> res0: Int = 144

	// Let's write a factorial function in terms of product.
 	def factorial(n: Int) = product(x => x)(1, n)
                                                  //> factorial: (n: Int)Int
 	factorial(3)                              //> res1: Int = 6
 	factorial(4)                              //> res2: Int = 24

	// A more general function that could be sum or product.

	// My attempt:
	def genfun(f: Int => Int)(op: (Int, Int) => Int)(unit: Int)(a: Int, b: Int): Int = {
		if (a > b) unit else op(f(a), genfun(f)(op)(unit)(a+1,b))
	}                                         //> genfun: (f: Int => Int)(op: (Int, Int) => Int)(unit: Int)(a: Int, b: Int)Int
                                                  //|

	// Odersky's solution:
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int =
  	if (a > b) unit
  	else combine(f(a), mapReduce(f, combine, unit)(a+1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, unit: Int)(a: Int, b:
                                                  //|  Int)Int
  def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
                                                  //> product2: (f: Int => Int)(a: Int, b: Int)Int

  product2(x => x * x)(3, 4)                      //> res3: Int = 144

  def sum(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x + y, 0)(a, b)
                                                  //> sum: (f: Int => Int)(a: Int, b: Int)Int
	sum(x => x * x)(3, 4)                     //> res4: Int = 25

}