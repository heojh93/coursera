object exercise2b {

  { // First Try

    // sum the integers between a and b (inclusive):
    def sumInts(a: Int, b: Int): Int =
      if (a > b) 0 else a + sumInts(a + 1, b)

    // sum the squares of integers between a and b (inclusive):
    def sumSquares(a: Int, b: Int): Int =
      if (a > b) 0 else a * a + sumSquares(a + 1, b)

    // sum the cubes of integers between a and b (inclusive):
    def sumCubes(a: Int, b: Int): Int =
      if (a > b) 0 else a * a * a + sumCubes(a + 1, b)
  }


  { // Second Try

    // The foregoing could have been implemented more elegantly as follows:

    // sum f(x) for all x between a and b (inclusive)
    def sum(f: Int => Int, a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sum(f, a + 1, b)

    // then sumInts, sumSquares, and sumCubes could have been implemented as

    def sumInts(a: Int, b: Int): Int = sum(id, a, b)
    def sumSquares(a: Int, b: Int): Int = sum(square, a, b)
    def sumCubes(a: Int, b: Int): Int = sum(cube, a, b)

    def id(a: Int): Int = a
    def square(a: Int): Int = a * a
    def cube(a: Int): Int = a * a * a

    sumSquares(0, 3)
    sumCubes(0, 3)

  }                                               //> res0: Int = 36

  { // Third Try

    // But even that is not great.  We don't need to name all these small functions.
    // We can use anonymous functions.
    // sum f(x) for all x between a and b (inclusive)
    def sum(f: Int => Int, a: Int, b: Int): Int =
      if (a > b) 0 else f(a) + sum(f, a + 1, b)

    def sumIntsAlt2(a: Int, b: Int): Int = sum((x: Int) => x, a, b)
    def sumSquaresAlt2(a: Int, b: Int): Int = sum((x: Int) => x * x, a, b)

    sumIntsAlt2(0, 3)
    sumSquaresAlt2(0, 3)
  }                                               //> res1: Int = 14


  { // Fourth Try

    // The sum function above uses linear recursion.  Let's write a tail recursive version.
    // Here the tail recursion actually benefits, unlike in the factorial case.
    def sum(f: Int => Int)(a: Int, b: Int): Int = {
      def sumAux(a: Int, acc: Int): Int = {
        if (a > b) acc else sumAux(a + 1, acc + f(a))
      }
      sumAux(a, 0)

    }
    // Note that we separated the argument list.
    // This is useful because now the following expression is valid:
    sum(x => x * x)_
  }                                               //> res2: (Int, Int) => Int = <function2>

	// This brings us to Currying

  { // Fifth Try

    // We implement the sum function as a function that returns another function:
    def sum(f: Int => Int): (Int, Int) => Int = {
      def sumAux(a: Int, b: Int): Int = {
        if (a > b) 0
        else f(a) + sumAux(a+1, b)
      }
      sumAux
    }
	  def sumSquares = sum(x => x*x)
	  sumSquares(0, 3)

	  // Or avoid the middle man and use the sum function directly:
	  sum(x=> x*x*x)(0, 3)
	  // So function application associates to the left.
  }                                               //> res3: Int = 36























} // End object