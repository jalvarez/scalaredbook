package scala.redbook.chapter5

trait Chapter5 {
  def constant[A](a: A): Stream[A] = {
    lazy val c: Stream[A] = Stream.cons[A](a, c)
    c
  }
  
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }
  
  def fibs: Stream[Int] = {
    def f(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a, f(b, a + b))
    }
    f(0,1)
  }
  
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => Stream.Empty
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
    }
  }
}