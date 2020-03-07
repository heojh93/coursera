object exercise3b {

{
  def product(f:Int => Int)(a: Int, b: Int): Int = {
  	if (a>b) 1
  	else f(a) * product(f)(a+1, b)
  }


  def factorial(b: Int): Int = product(x=>x)(1,b)

  // Test them:
  product(x=>x)(1,6)
	//factorial(6)
 }                                                //> res0: Int = 720


 {
 // Let's write an abstract function that could be used to
 // construct the sum and product functions.  The idea is
 // to abstract out the basic elements. When we implemented the
 // sum and product functions, there was a map step, provided
 // by the function f, and a reduce step, which was + in the sum
 // case and * in the product case.

	// Applies map to each integer between a and b and then
	// combine the results using the reduce function.
  def mapReduce(map: Int => Int, reduce: (Int, Int) => Int, unit: Int)(a: Int, b: Int): Int = {
  	if(a > b) unit
  	else reduce(map(a), mapReduce(map, reduce, unit)(a+1, b))
  }

	// Test it:
	mapReduce(x => x*x, (u, v) => u+v, 0)(0, 3)

	// Now implement product using mapReduce
	def product(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (u,v)=>u*v, 1)(a,b)
	def factorial(n: Int): Int = mapReduce(x=>x, (u,v)=>u*v, 1)(1,n)
	// Test it:
	product(x=>x)(1,6)
	factorial(6)

	}                                         //> res1: Int = 720
}


object convolver {

	// Applies map to each integer between a and b and then
	// combine the results using the reduce function.
  def mapReduce(map: Int => Double, reduce: (Double, Double) => Double, unit: Double)(a: Int, b: Int): Double = {
  	if(a > b) unit
  	else reduce(map(a), mapReduce(map, reduce, unit)(a+1, b))
  }

	// The binary operation defined on the group (customize this)
	def binop(x: Int, y: Int): Int = x+y

	// The inverse operation defined on the group (customize this)
	def inverse(x: Int) = -x

	// Generalized translation (no need to customize)
	def trans(g: Int => Double)(y: Int): Int => Double = {
		x => g( binop( inverse(y), x ) )  // For example: x => g(x-y)
	}

	// The map used inside the sum in the convolution
	def map(f: Int => Double, g: Int => Double, x: Int): Int=>Double = {
		y => f(y)*(trans(g)(y))(x)  // For example: f(y) * g(x-y)
	}

	def conv(f: Int => Double)(g: Int => Double): Int => Double = {
		x => mapReduce(map(f, g, x), (a, b) => a+b, 0)(0, 15)
	}





















}