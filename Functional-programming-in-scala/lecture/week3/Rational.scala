package week3

class Rational(x: Int, y: Int) {

	require(y != 0, "denominator must not be zero")
	// require is used to prevent illegal arguments; assert is used to check code for errors

	// When `this` is called as a function, it becomes a second constructor.
	def this(x: Int) = this(x, 1)
	
	private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
	val numer = x/gcd(x, y)
	val denom = y/gcd(x, y)

	override def toString = numer + "/" + denom

	def < (that: Rational) = numer * that.denom < that.numer * denom

	def max(that: Rational) = if (this < that) that else this

	def + (that: Rational) =
		new Rational(
			numer * that.denom + that.numer * denom,
			denom * that.denom)

	def * (that: Rational) = new Rational(numer*that.numer, denom*that.denom)

	def - (that: Rational) = this + -that
	
	def unary_- : Rational = new Rational(-numer, denom)


}