package scala.redbook.chapter6

import scala.annotation.tailrec

trait Chapter6 {
  trait RNG {
    def nextInt: (Int, RNG)
  }
  
  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }
  
  @tailrec
  final def nonNegative(rng: RNG): (Int, RNG) = {
    rng.nextInt match {
      case (i, newRng) if (i >= 0) => (i, newRng)
      case (_, newRng) => nonNegative(newRng)
    }
  }
  
  def double(rng: RNG): (Double, RNG) = {
    val (i, newRng) = nonNegative(rng)
    (i.toDouble / Int.MaxValue, newRng)
  }
  
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, newRng) = rng.nextInt
    val (d, newRng2) = double(newRng)
    ((i, d), newRng2)
  }
  
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, newRng) = double(rng)
    val (i, newRng2) = newRng.nextInt
    ((d, i), newRng2)
  }
  
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, newRng) = double(rng)
    val (d2, newRng2) = double(newRng)
    val (d3, newRng3) = double(newRng2)
    
    ((d1, d2, d3), newRng3)
  }
  
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    
    @tailrec
    def randomInts(generated: List[Int], count: Int, rng: RNG): (List[Int], RNG) = {
      count match {
        case 0 => (generated, rng)
        case 1 =>
          val (i, newRng) = rng.nextInt
          (i :: generated, newRng)
        case n =>
          val (i, newRng) = rng.nextInt
          randomInts(i :: generated, n - 1, newRng)
      }
    }
      
    randomInts(Nil, count, rng)  
  }
}