object exercise2 {

	// First we define a simple function for adding the integers between a and b.
  def sumInts(a: Int, b: Int): Int =
  	if (a > b) 0 else a + sumInts(a + 1, b)   //> sumInts: (a: Int, b: Int)Int

	// Try it out:
  sumInts(1,3)                                    //> res0: Int = 6


	// Next we define a function for computing the sum of cubes.
	def cube(x: Int): Int = x * x * x         //> cube: (x: Int)Int

	def sumCubes(a: Int, b: Int): Int =
		if (a > b) 0 else cube(a) + sumCubes(a + 1, b)
                                                  //> sumCubes: (a: Int, b: Int)Int
	// Try it out:
	sumCubes(1,3)                             //> res1: Int = 36


	// Finally, define a sum of factorials function:
	def factorial(a: Int): Int =
		if (a == 1) 1 else a*factorial(a-1)
                                                  //> factorial: (a: Int)Int
	def sumFactorials(a: Int, b: Int): Int =
		if (a > b) 0 else factorial(a) + sumFactorials(a+1, b)
                                                  //> sumFactorials: (a: Int, b: Int)Int
	// Try it out:
	sumFactorials(1,3)                        //> res2: Int = 9


	// We see a general pattern here: \sum_{n=a}^b f(n)
	// Let us now factor out the common pattern into a single method.
	def sum(f: Int => Int, a: Int, b: Int): Int =
		if (a > b) 0 else f(a) + sum(f, a+1, b)
                                                  //> sum: (f: Int => Int, a: Int, b: Int)Int

	// We now define the sumInts, sumCubes, and sumFactorials
	// this time using our new sum function which takes a function as an argument.
	def sumInts2(a: Int, b: Int): Int = sum(x => x, a, b)
                                                  //> sumInts2: (a: Int, b: Int)Int
	def sumCubes2(a: Int, b: Int): Int = sum(cube, a, b)
                                                  //> sumCubes2: (a: Int, b: Int)Int
	def sumFactorials2(a: Int, b: Int): Int = sum(factorial, a, b)
                                                  //> sumFactorials2: (a: Int, b: Int)Int

	// Now we improve this with the use of anonymous functions.
	def sumCubesAlt(a: Int, b: Int): Int = sum((x: Int) => x*x*x, a, b)
                                                  //> sumCubesAlt: (a: Int, b: Int)Int
	// Note: sumInts2 already used this strategy.
	// Also note, we don't really need to specify the type of x.

  // The general form of an anonymous function is
  //  (x: T1, y: T2, z:T3) => E
  // where T1, T2, T3 specifies the types and E is an expression
  // presumably involving x, y, and z.


  // ---- A tail recursive version of sumInts
  def sumTR(f: Int => Int, a: Int, b: Int) = {
  	def loop(a: Int, acc: Int): Int =
  		if (a > b) acc
  		else loop(a+1, f(a) + acc)
  	loop(a, 0)
  }                                               //> sumTR: (f: Int => Int, a: Int, b: Int)Int

  // Try it out:
  sumTR(x => x*x, 3,5)                            //> res3: Int = 50


  // ======  LECTURE 2.2: Currying =======
  //
  // Consider the sumInts2, sumCubes2, sumFactorials2 functions above.
  // These all take a and b arguments and do nothing with them but pass them.
  // Here we see how we can get rid of these parameters.

  // First, we rewrite sum to be a function that returns another function.
  // It will return sumF which has type (Int, Int) => Int.  That is, sumF
  // takes two integer arguments and returns an integer:
  def sum2(f: Int => Int): (Int, Int) => Int = {
  	def sumF(a: Int, b: Int): Int =
  		if (a > b) 0 else f(a) + sumF(a+1, b)
  	sumF   // Don't forget that sum2 should return the function!!
  }                                               //> sum2: (f: Int => Int)(Int, Int) => Int

	// Now we can redefine sumInts, sumCubes, and sumFactorial as follows:
	def sumInts3 = sum2(x => x)               //> sumInts3: => (Int, Int) => Int
	def sumCubes3 = sum2(x => x*x*x)          //> sumCubes3: => (Int, Int) => Int
	def sumFactorials3 = sum2(factorial)      //> sumFactorials3: => (Int, Int) => Int

  // Try them out:
  sumInts3(1,3)                                   //> res4: Int = 6
  sumCubes3(1,3)                                  //> res5: Int = 36
  sumFactorials3(1,3)                             //> res6: Int = 9

	// Next we see how we can avoid the "middle man"
	// That is, instead of defining sumInts3, sumCubes3, etc. let us just use
	// the sum2 function directly:
	sum2(x=>x)(1,3)                           //> res7: Int = 6
	sum2(cube)(1,3)                           //> res8: Int = 36
	sum2(factorial)(1,3)                      //> res9: Int = 9

  // Aside: this will come in handy when we do convolution; we should
  // be able to write the "convolution by f of g at x" as follows:
  // convolution(f)(g)(x)
  // It will look something like this, I think:
  //  def convolution(f: Int => Double): (Int => Double) => (Int => Double) = { ... }
  // But see below.

  // Now we rewrite sum again, this time with multiple parameter lists.
  def sum3(f: Int => Int)(a: Int, b: Int): Int =
  	if (a > b) 0 else f(a) + sum3(f)(a+1, b)  //> sum3: (f: Int => Int)(a: Int, b: Int)Int

  // Try it out:
	sum3(x=>x)(1,3)                           //> res10: Int = 6
	sum3(cube)(1,3)                           //> res11: Int = 36
	sum3(factorial)(1,3)                      //> res12: Int = 9

	def sum4 = sum3(cube)_                    //> sum4: => (Int, Int) => Int

  sum4(1,3)                                       //> res13: Int = 36

  // Note that the type of the sum3 function is
  //   (Int => Int) => (Int, Int) => Int
  // since functional types ASSOCIATE TO THE RIGHT, so this is equivalent to
  //   (Int => Int) => ((Int, Int) => Int)
  //
  // More generally, we can define functions with multiple parameter lists as follows:
  //   def f(args1)(args2)(args3)(args4) = E
  // where E is some expression involving the argument lists.
  // Note: this is equivalent to
  //   def f(args1)(args2)(args3) = (args4 => E)
  // because we f(args1)(args2)(args3) can be thought of as a function that takes
  // the last argument list, args4, and returns E.
  // Another equivalent expression is
  //
  //     def f = (args1 => (args2 => (args3 => (args4 => E) ) ) )
  //
  // This is called "Currying" (named after Haskel Curry.

  // -----------------------------------------
  // CONVOLUTION: here's how we should do it
  def convolution(f: Int => Double)(g: Int => Double)(x: Int): Double = {
  	0.0  // insert appropriate code for convolution here
  }                                               //> convolution: (f: Int => Double)(g: Int => Double)(x: Int)Double
  // This way, given some function f, we can define,
  //   def Cf = convolution(f)_
  // and then, given another function g, we can do this:
  //   def fg = Cf(g)
  // and finally, we have the convolution as a function, which we can evaluate at various points:
  //   fg(x)

}