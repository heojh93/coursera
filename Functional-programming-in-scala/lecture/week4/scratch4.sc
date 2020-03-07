import week4.List

object scratch {
  val x = List(1, 2)                              //> x  : week4.List[Int] = week4.Cons@4ec44deb
  x.tail.head                                     //> res0: Int = 2
  val y = List(3)                                 //> y  : week4.List[Int] = week4.Cons@6ea4ce0d
  y.head                                          //> res1: Int = 3
}