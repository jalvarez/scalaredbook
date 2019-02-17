package scala.redbook.chapter6

import org.scalatest.Matchers
import org.scalatest.WordSpec

class PureFunctionalStateSpec extends WordSpec with Matchers with Chapter6 {
  "In chapter 6, about pure functional random number generator" when {
    val beNonNegative = be >= 0 and be <= Int.MaxValue
    
    "resolve exercise 6.1" must {
      "implement a function that generate a random integer between 0 and Int.maxValue (inclusive)" in {
        val seed = Int.MinValue
        val rng = SimpleRNG(seed)
        val (aNonNegative, _) = nonNegative(rng)
        aNonNegative should beNonNegative
      }
    }

    val bePositiveLessThatOne = be >= 0.0 and be <= 1.0
    
    "resolve exercise 6.2" must {
      "implement a function that generate a double between 0 and 1 (not included)" in {
        val seed = 1
        val rng = SimpleRNG(seed)
        val (aDouble, _) = double(rng)
        aDouble should bePositiveLessThatOne
      }
    }
    
    "resolve exercise 6.3" must {
      "implement a function that generate an (Int, Double) pair" in {
        val seed = 1
        val rng = SimpleRNG(seed)
        val ((aInt, aDouble), _) = intDouble(rng)
        aDouble should bePositiveLessThatOne
      }
      
      "implement a function that generate an (Double, Int) pair" in {
        val seed = 1
        val rng = SimpleRNG(seed)
        val ((aDouble, aInt), _) = doubleInt(rng)
        aDouble should bePositiveLessThatOne
      }
      
      "implement a function that generate an Double 3-tuple" in {
        val seed = 1
        val rng = SimpleRNG(seed)
        val ((aDouble, otherDouble, otherDoubleMore), _) = double3(rng)
        aDouble should bePositiveLessThatOne
        otherDouble should bePositiveLessThatOne
        otherDoubleMore should bePositiveLessThatOne
      }
    }
    
    "resolve exercise 6.4" must {
      "implement a function that generate a list of random integers" in {
        val seed = 1
        val numberOfIntegers = 10
        val rng = SimpleRNG(seed)
        val (randomInts, _) = ints(numberOfIntegers)(rng)
        
        randomInts should have size 10
      }
    }
  }
}