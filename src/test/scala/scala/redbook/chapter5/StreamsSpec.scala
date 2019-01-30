package scala.redbook.chapter5

import org.scalatest.Matchers
import org.scalatest.WordSpec

class StreamsSpec extends WordSpec with Matchers with Chapter5 {
  val beginOfFibonacciSeq = Stream(0, 1, 1, 2, 3, 5, 8, 13, 21, 34)
  
  "In chapter five" when { 
    "resolve exercise 5.8 a constant stream" must {
      "return always the same constant" in {
        constant(5).take(3) shouldBe Stream(5, 5, 5)
      }
    }
    
    "resolve exercise 5.9 a stream from a integer number" must {
      "return a stream that begin in this number" in {
        from(5).take(3) shouldBe Stream(5, 6, 7)
      }
    }
    
    "resolve exercise 5.10 the fibonacci stream" must {
      s"return a stream that begin with ${beginOfFibonacciSeq.force}" in {
        fibs.take(beginOfFibonacciSeq.size) shouldBe beginOfFibonacciSeq
      }
    }
    
    "resolve exercise 5.11 and 5.12, the unfold method" must {
      "allow implement the ones function that return one always" in {
        def ones = unfold(1) { _ => Some((1,1)) }
        ones.take(3) shouldBe Stream(1, 1, 1)
      }
      
      "allow implement the constant function that return a constant always" in {
        def constant(c: Int) = unfold(c) { _ => Some((c,c)) }
        constant(5).take(3) shouldBe Stream(5, 5, 5)
      }
      
      "allow implement the from function that begin in a number" in {
        def from(n: Int) = unfold(n) { i => Some((i, i + 1)) }
        from(5).take(3) shouldBe Stream(5, 6, 7)
      }
      
      "allow implement the fibs function that that with fibonacci sequence" in {
        def fibs: Stream[Int] = unfold((0,1)) { case (a, b) => Some((a, (b, a + b))) }
        fibs.take(beginOfFibonacciSeq.size) shouldBe beginOfFibonacciSeq
      }
    }
    
    "resolve exercise 5.13, the unfold method" must {
      "allow implement the map function that apply a function over all stream elements" in {
        val aStream = Stream(1, 2, 3)
        def map[A, B](s: Stream[A])(f: A => B): Stream[B] = {
          unfold(s) {
            _ match {
              case Stream.Empty => None
              case Stream.cons(h, t) => Some((f(h), t))
            }
          }
        }
        
        map(aStream) { _ * 2 } shouldBe aStream.map { _ * 2 }
      }
      
      "allow implement the take function that take the first n elements of stream" in {
        val aInfiniteStream = constant(1)
        def take[A](s: Stream[A], n: Int): Stream[A] = {
          unfold((s,n)) {
            _ match {
              case (_, 0) => None
              case (Stream.Empty, _) => None
              case (Stream.cons(h, t), n) => Some((h, (t, n - 1)))
            }
          }
        }
        
        take(aInfiniteStream, 3).size shouldBe 3
      }
      
      "allow implement the zipAll method that combine two streams" in {
        val streamA = from(1).take(3)
        val streamB = from(10).take(2)
        
        def zipAll[A, B](s1: Stream[A], s2: Stream[B]): Stream[(Option[A], Option[B])] = {
          unfold((s1, s2)) {
            _ match {
              case (Stream.Empty, Stream.Empty) => None
              case (Stream.cons(h, t), Stream.Empty) => Some(((Some(h), None), (t, Stream.Empty)))
              case (Stream.Empty, Stream.cons(h, t)) => Some(((None, Some(h)), (Stream.Empty, t)))
              case (Stream.cons(h1, t1), Stream.cons(h2, t2)) => Some(((Some(h1), Some(h2)), (t1, t2)))
            }
          }
        }
        
        zipAll(streamA, streamB) shouldBe Stream((Some(1), Some(10)), (Some(2), Some(11)), (Some(3), None))
      }
    }
  }
}