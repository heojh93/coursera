package week6

object combinations {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  // Example: Combinations (17'11")
	// List all combinations of numbers x and y where x is drawn from 1..M and y from 1..N
	val M = 3; val N = 4                      //> M  : Int = 3
                                                  //| N  : Int = 4
	// First recall what the map function returns:
	// For a given number, say 8, we have
	(0 until N) map (y => (8, y))             //> res0: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((8,0), (8,1
                                                  //| ), (8,2), (8,3))
	
	// Now produce such a vector of pairs, for each x in (0 until M) and concatenate with flatMap:
	(0 until M) flatMap ( x => (0 until N) map (y => (x, y)) )
                                                  //> res1: scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((0,0), (0,1
                                                  //| ), (0,2), (0,3), (1,0), (1,1), (1,2), (1,3), (2,0), (2,1), (2,2), (2,3))

	// Using map instead of flatMap produces a vector of vectors of pairs---not what we want:
	(0 until M) map ( x => (0 until N) map (y => (x, y)) )
                                                  //> res2: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Index
                                                  //| edSeq[(Int, Int)]] = Vector(Vector((0,0), (0,1), (0,2), (0,3)), Vector((1,0)
                                                  //| , (1,1), (1,2), (1,3)), Vector((2,0), (2,1), (2,2), (2,3)))
  
}