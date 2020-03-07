sealed trait LazyList[+A] {

}

case object Final extends LazyList[Nothing]

case class #:::[A](head: () => A, tail: () => LazyList[A]) extends LazyList[A]

object LazyList{

  def apply[A](as: A*): LazyList[A] =
    if(as.isEmpty)
      Final
    else
      ::->(as.head, apply(as.tail:_*))

  private def ::->[A](head1: => A, tail1: => LazyList[A]): LazyList[A] = {
    lazy val head = head1
    lazy val tail = tail1
    #:::(() => head, () => tail)
  }

}
object LazyListFunctions {
  def map[A, B](f: A => B, l: LazyList[A]): LazyList[B] = l match {
    case Final => Final
    case a #::: as => #:::(() => f(a()), () => map(f, as()))
  }

  def count[A](l: LazyList[A]): Int = l match {
    case Final => 0
    case a #::: as => 1 + count(as())
  }
}

val a = LazyList()
`