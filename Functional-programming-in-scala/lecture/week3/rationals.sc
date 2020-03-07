// Lecture 2.5 -- Functions and Data
// Lecture 2.6 -- More Fun With Rationals
// Lecture 2.7 -- Evaluation and Operators
object rationals {
  val x = new Rational(1, 3)                      //> x  : week3.Rational = 1/3
  val y = new Rational(5, 7)                      //> y  : week3.Rational = 5/7
  val z = new Rational(3, 2)                      //> z  : week3.Rational = 3/2
	x.sub(y).sub(z)                           //> res0: week3.Rational = -79/42
	x.add(y).mul(z)                           //> res1: week3.Rational = 11/7
	y.add(y)                                  //> res2: week3.Rational = 10/7
	x.less(y)                                 //> res3: Boolean = true
	x.max(y)                                  //> res4: week3.Rational = 5/7
	
	// Any function with a parameter can be used in infix notation:
	// Examples:
	x max y                                   //> res5: week3.Rational = 5/7
	x less y                                  //> res6: Boolean = true
	x sub y sub z                             //> res7: week3.Rational = -79/42
	
	// Demonstrating the new identifiers
	x Max y                                   //> res8: week3.Rational = 5/7
	x < y                                     //> res9: Boolean = true
	x - y - z                                 //> res10: week3.Rational = -79/42

	// Note that x.add(y).mul(z) is the same as (x+y)*z, NOT x + y * z.
	x.add(y).mul(z)                           //> res11: week3.Rational = 11/7
	x + y * z                                 //> res12: week3.Rational = 59/42
	(x + y) * z                               //> res13: week3.Rational = 11/7
}

class Rational(x: Int, y: Int) {
	// A requirement that throws illegal argument exception
	require(y != 0, "denominator must not be zero")
	// require is used to prevent illegal arguments; assert is used to check code for errors

	// When `this` is called as a function, it becomes a second constructor.
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
	// First implementation:
	// g = gcd(x, y)
	// def numer = x/g
	// def denom = y/g
  // Alternative implementation:
  val numer = x/gcd(x, y)
	val denom = y/gcd(x, y)

	def less(that: Rational) = numer * that.denom < that.numer * denom

	def max(that: Rational) = if (this.less(that)) that else this
	
	def add(that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)
			
	def mul(that: Rational) = new Rational(numer*that.numer, denom*that.denom)

	def neg = new Rational(-numer, denom)
	
	def sub(that: Rational) = add(that.neg)
	
	override def toString = numer/gcd(numer, denom) + "/" + denom
	// We could instead keep rational numbers unsimplified internally, and
	// simplify only when printing, but this is more likely to lead to overflow.
	// override def toString = numer/gcd(numer, denom) + "/" + denom/gcd(numer, denom)

	// Lecture 2.7 -- Evaluation and Operators
	// We can define methods with identifiers that are more natural when using infix notation:

	def < (that: Rational) = numer * that.denom < that.numer * denom

	def Max(that: Rational) = if (this < that) that else this

	def + (that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)

	def * (that: Rational) = new Rational(numer*that.numer, denom*that.denom)

	def - (that: Rational) = add(that.neg)
	
	// We can also replace `neg`, but we need to specify that it is a unary operator:
	def unary_- : Rational = new Rational(-numer, denom)

	// Or, simply,
	//	 def unary_- = new Rational(-numer, denom)

	// NOTE: Precedence of an operator is determined by the operator's FIRST character.
	//
	// Exercise: provide a fully parenthesized version of the following expression:
	//     a + b ^? c ?^ d less a ==> b | c
	// Answer:
	//     Highest to lowest: ?, +, =, ^, |, less
	//     ((a + b) ^? (c ?^ d)) less ((a ==> b) | c)





	
}