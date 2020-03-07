object session {

  1 + 2                                           //> res0: Int(3) = 3

  def square(x: Double) = { x * x }               //> square: (x: Double)Double

  // Here you want second argument to be call-by-name instead of
  // call-by-value so that and(false, loop) terminates and returns false
  // (this is why => appears in second argument)
  def and(x: Boolean, y: => Boolean) = if (x) y else false
                                                  //> and: (x: Boolean, y: => Boolean)Boolean

  // Here you want second argument to be call-by-name so that
  // or(true, loop) terminates and returns true
  def or(x: Boolean, y: => Boolean) = if (x) true else y
                                                  //> or: (x: Boolean, y: => Boolean)Boolean
  def abs(x: Double) = if (x < 0) -x else x       //> abs: (x: Double)Double

  def sqrt(x: Double) = {

    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))

    def isGoodEnough(guess: Double) =
      abs(guess * guess - x) / x < 1e-6

    def improve(guess: Double) =
      (guess + x / guess) / 2

    sqrtIter(1.0)
  }                                               //> sqrt: (x: Double)Double

  def loop: Boolean = loop                        //> loop: => Boolean
  def x = loop                                    //> x: => Boolean
  or(true, x)                                     //> res1: Boolean = true
  and(false, x)                                   //> res2: Boolean = false

  square(2)                                       //> res3: Double = 4.0
  square(3)                                       //> res4: Double = 9.0
  sqrt(2)                                         //> res5: Double = 1.4142135623746899
  sqrt(4)                                         //> res6: Double = 2.0000000929222947
  sqrt(1e-6)                                      //> res7: Double = 0.0010000001533016628
  sqrt(1e60)                                      //> res8: Double = 1.0000000031080746E30

}