package week5

// Lecture 5.4 -- Higher Order List Functions
//
object map {

	// Square each element of a list
	def squareList(xs: List[Int]): List[Int] = xs match {
		case Nil => xs
		case y :: ys => (y*y) :: squareList(ys)
		}                                 //> squareList: (xs: List[Int])List[Int]

	def squareListmap(xs: List[Int]): List[Int] = xs map(x => x*x)
                                                  //> squareListmap: (xs: List[Int])List[Int]

	// Find positive elements of a list: 1st implementation
  def posElems(xs: List[Int]): List[Int] = xs match {
  	case Nil => xs
  	case y :: yt => if(y > 0) y :: posElems(yt) else posElems(yt)
  	}                                         //> posElems: (xs: List[Int])List[Int]

	// Find elements in a list satisfying some condition p(x).
  def myfilter[T](xs: List[T])(p: T => Boolean): List[T] = xs match {
  	case Nil => xs
  	case y :: yt => if(p(y)) y :: myfilter(yt)(p) else myfilter(yt)(p)
  	}                                         //> myfilter: [T](xs: List[T])(p: T => Boolean)List[T]

	// Find positive elements of a list: 2nd implementation using myfilter function
  def posElems2(xs: List[Int]): List[Int] = myfilter(xs)(x => x>0)
                                                  //> posElems2: (xs: List[Int])List[Int]
  	
	// Find positive elements of a list: 3rd implementation using the filter function of Scala's List class.
  def posElems3(xs: List[Int]): List[Int] = xs filter (x => x>0)
                                                  //> posElems3: (xs: List[Int])List[Int]

                                                  
  val mylist = List(-2, 0, 2, 1, -3, 5)           //> mylist  : List[Int] = List(-2, 0, 2, 1, -3, 5)
  squareList(mylist)                              //> res0: List[Int] = List(4, 0, 4, 1, 9, 25)
  squareListmap(mylist)                           //> res1: List[Int] = List(4, 0, 4, 1, 9, 25)

	// Various ways to find the positive elements in a list
  posElems(mylist)                                //> res2: List[Int] = List(2, 1, 5)
  myfilter(mylist)(x => x>0)                      //> res3: List[Int] = List(2, 1, 5)
  posElems2(mylist)                               //> res4: List[Int] = List(2, 1, 5)
  mylist filter (x => x>0)                        //> res5: List[Int] = List(2, 1, 5)
  posElems3(mylist)                               //> res6: List[Int] = List(2, 1, 5)
}