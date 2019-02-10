package scala.redbook.chapter5

import scala.annotation.tailrec

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

  @tailrec
  final def last[A](s: Stream[A]): Option[A] = {
    s match {
      case Stream.Empty => None
      case Stream.cons(h, Stream.Empty) => Some(h)
      case Stream.cons(_, t) => last(t)
    }
  }
  
  def startsWith[A](s: Stream[A], prefix: Stream[A]): Boolean = {
    val doubleEmpty = (Stream.empty, Stream.empty)
    last(unfold((s, prefix)) {
            _ match {
              case (Stream.Empty, Stream.Empty) => None
              case (Stream.cons(_, _), Stream.Empty) => Some((true, doubleEmpty))
              case (Stream.Empty, Stream.cons(_, _)) => Some((false, doubleEmpty))
              case (Stream.cons(h1, t1), Stream.cons(h2, t2)) =>
                val areEquals = h1 == h2 
                Some((areEquals, if (areEquals) (t1, t2) else doubleEmpty))
            }
        }).getOrElse(false)
  }
  
  def tails[A](stream: Stream[A]): Stream[Stream[A]] = {
    unfold(Some(stream): Option[Stream[A]]) {
      _ match {
        case None => None
        case Some(Stream.Empty) => Some((Stream.Empty, None))
        case Some(s @ Stream.cons(_, t)) => Some((s, Some(t)))
      }
    }
  }
  
  def scanRight[A](stream: Stream[A], z: A)(f: (A, A) => A): Stream[A] = {
    def sr(stream: Stream[A], f: (A, A) => A): Stream[A] = {
      stream match {
        case Stream.Empty => Stream(z)
        case Stream.cons(h, t) =>
          val s1 = sr(t, f)
          val Stream.cons(h1, t1) = s1
          Stream.cons(f(h, h1), s1)
      }
    }
    
    sr(stream, f)
  }
}