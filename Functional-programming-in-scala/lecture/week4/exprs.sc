package week4

object exprs {
	def show(e: Expr): String = e match {
		case Number(x) => x.toString
		case Sum(e1, e2) => show(e1) + " + " + show(e2)
		case Prod(e1: Sum, e2: Sum) => "( " + show(e1) + " ) * ( " + show(e2) + " )"
		case Prod(e1, e2: Sum) => show(e1) + " * ( " + show(e2) + " )"
		case Prod(e1: Sum, e2) => "( " + show(e1) + " ) * " + show(e2)
		case Prod(e1, e2) => show(e1) + " * " + show(e2)
		case Var(x) => x
	}                                         //> show: (e: week4.Expr)String

	val s = Sum(Sum(Number(1),Number(2)),Number(3))
                                                  //> s  : week4.Sum = Sum(Sum(Number(1),Number(2)),Number(3))
	show(s)                                   //> res0: String = 1 + 2 + 3
	s.show                                    //> res1: String = " (  ( 1 + 2 )  + 3 ) "
	
	show(Sum(Number(1), Number(44)))          //> res2: String = 1 + 44
	
	show(Sum(Prod(Number(2), Var("x")), Var("y")))
                                                  //> res3: String = 2 * x + y

	show(Prod(Sum(Number(2), Var("x")), Var("y")))
                                                  //> res4: String = ( 2 + x ) * y

}