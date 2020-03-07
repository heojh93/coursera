package week4

// Lecture 3.3 -- Polymorphism
trait List[+T] {
	def isEmpty: Boolean  // what kind of list, Empty or Cons?
	def head: T           // the first element of the list
	def tail: List[T]     // the remainder
	def getElem(n: Int): T
	// Lecture 4.4 -- Variance
	def prepend [U >: T](elem: U): List[U] = new Cons(elem, this)

}

class Cons[T](val head: T, val tail: List[T]) extends List[T] {
	// Recall: val is evaluated once; def is evaluated each time it is referenced.
	def isEmpty = false
	def getElem(n: Int): T = {
		if (n<0) throw new IndexOutOfBoundsException
		else if (n==0) head
		else if (tail isEmpty) throw new IndexOutOfBoundsException
		else tail getElem(n-1)
	}
}


// Lecture 4.4 -- Variance
// New implementation of Nil

// OLD
//class Nil[T] extends List[T] {
//  def isEmpty: Boolean = true
//  def head: Nothing = throw new java.util.NoSuchElementException("Nil.head")
//  def tail: Nothing = throw new java.util.NoSuchElementException("Nil.tail")
//}

// NEW
object Nil extends List[Nothing] {
  def isEmpty: Boolean = true
  def head: Nothing = throw new NoSuchElementException("Nil.head")
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
  def getElem(n: Int): Nothing = throw new NoSuchElementException("Nil.getElem")
}

object test {
  val x: List[String] = Nil
  def f(xs: List[NonEmpty]) = xs prepend Empty
}


// After redefining Nil as an object, instead of a class, 
// this List object must also be redefined.
//object List {
//  // List(1, 2) = List.apply(1, 2)
//  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
//  def apply[T](x: T): List[T] = new Cons(x, new Nil)
//  def apply[T]() = new Nil
//}
object List {
  // List(1, 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, Nil))
  def apply[T](x: T): List[T] = new Cons(x, Nil)
  def apply[T]() = Nil
  
}

