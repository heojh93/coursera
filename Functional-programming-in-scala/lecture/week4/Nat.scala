package week4

// Lecture 4.2 -- Objects Everywhere

// Peano numbers -- constructing natural numbers without primitive types.
abstract class Nat {
	def isZero: Boolean
	def predecessor: Nat
	def successor = new Succ(this)
	def + (that: Nat): Nat
	def - (that: Nat): Nat
}

object Zero extends Nat {
	def isZero = true
	def predecessor = throw new Error("0.predecessor")
	// def successor = new Succ(this)  (moved to superclass)
	def +(that: Nat) = that
	def -(that: Nat) = if (that.isZero) this else throw new Error("0-negative")
}

class Succ(n: Nat) extends Nat {
	def isZero = false
	def predecessor = n
	// def successor = new Succ(this) (moved to superclass)
	def +(that: Nat) = new Succ(n + that)
	def -(that: Nat) = if (that.isZero) this else n - that.predecessor
	
}