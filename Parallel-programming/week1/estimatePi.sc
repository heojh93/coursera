import javafx.concurrent.Task

import scala.util.Random

def mcCount(iter: Int): Int = {
  val randomX = new Random
  val randomY = new Random

  var hits = 0
  for (i <- 0 until iter) {
    val x = randomX.nextDouble()
    val y = randomY.nextDouble()
    if (x * x + y * y < 1) hits = hits + 1
  }
  hits
}

def monteCarloPiSeq(iter: Int): Double = 4.0 * mcCount(iter) / iter

monteCarloPiSeq(1000)
//res0: Double = 3.144

monteCarloPiSeq(10000)
//res1: Double = 3.1592

monteCarloPiSeq(100000)
//res2: Double = 3.1414

def task[A](c: => A): Task[A]

trait Task[A] {
  def join: A
}

def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
  val tB: Task[B] = task {
    taskB
  }
  val tA: A = taskA(tA, tB.join)
}

def monteCarloPiPar(iter: Int): Double = {
  val ((pi1, pi2), (pi3, pi4)) = parallel
}