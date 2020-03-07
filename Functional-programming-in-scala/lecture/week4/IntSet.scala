package week4

// All of this stuff was taken from the week3/intsets.sc file

// Lecture 3.1 Class Hierarchies
// In this lecture, we implement sets as binary trees.

object intsets {
	//val t1 = new NonEmpty(3, new Empty, new Empty)
	val t1 = new NonEmpty(3, Empty, Empty)    //> t1  : week3.NonEmpty = {.3.}
	val t2 = t1.incl(4)                       //> t2  : week3.IntSet = {.3{.4.}}
	Empty contains 1                          //> res0: Boolean = false
	t1 union t2                               //> res1: week3.IntSet = {.3{.4.}}

	val a = new NonEmpty(3, Empty, Empty)     //> a  : week3.NonEmpty = {.3.}
	val b = new NonEmpty(4, Empty, Empty)     //> b  : week3.NonEmpty = {.4.}
	
	b incl 3                                  //> res2: week3.IntSet = {{.3.}4.}
	a union b                                 //> res3: week3.IntSet = {{.3.}4.}
	
	val A = new NonEmpty(2, new NonEmpty(3, Empty, Empty), new NonEmpty(4, Empty, Empty))
                                                  //> A  : week3.NonEmpty = {{.3.}2{.4.}}
	val B = new NonEmpty(9, new NonEmpty(5, Empty, Empty), Empty)
                                                  //> B  : week3.NonEmpty = {{.5.}9.}
	val C = new NonEmpty(7, new NonEmpty(8, Empty, Empty), B)
                                                  //> C  : week3.NonEmpty = {{.8.}7{{.5.}9.}}
	A union C                                 //> res4: week3.IntSet = {{{{.2.}3{.4.}}8.}7{{.5.}9.}}
}

// IntSet is the superclass of Empty and NonEmpty classes (defined below)
abstract class IntSet {

	// Returns True if the element x is contained in the set.
	def contains(x: Int): Boolean

  // Insert the element x into the set.
	// (Actually, create a new set starting with the old and adding x.)
	def incl(x: Int): IntSet
	// (Shouldn't we call this `insert`)

	def union(that: IntSet): IntSet

}

// Concept: persistent data structures
// The old version of the data structure is maintained;
// instead of changing it, a new one is created.


// Class Extensions
// Empty and NonEmpty will be subclasses of IntSet

// Actually, there is no reason to make Empty a class.  We only need one empty set.
//class Empty extends IntSet {
// 	def contains(x: Int): Boolean = false
//	def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
//	override def toString = "."
//}


// We only need one empty set, so we make it an object instead of a class.
object Empty extends IntSet {
	def contains(x: Int): Boolean = false
	//def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
	// Since `Empty` is already an object, we don't need `new`
	def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
	def union(that: IntSet): IntSet = that
	override def toString = "."
}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

	// check if x is somewhere in the tree
	def contains(x: Int): Boolean =
		if (x < elem) left contains x  // same as this.left.contains(x)
		else if (x > elem) right contains x
		else true

	// Insert x into the set.  (Perhaps clearer to call this method insert.)
	def incl(x: Int): IntSet =
		// Instead of modifying the original object, we create a new one.
		if (x < elem) new NonEmpty(elem, left incl x, right)
		else if (x > elem) new NonEmpty(elem, left, right incl x)
		else this
	// But what if x is already in the set, but not equal to elem?
	// Won't there then be two occurrences of x in the set? Multiset?


	def union(that: IntSet): IntSet =
		((left union right) union that) incl elem


	override def toString = "{" + left + elem + right + "}"

}