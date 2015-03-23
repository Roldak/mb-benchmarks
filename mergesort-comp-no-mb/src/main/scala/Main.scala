package miniboxing.example

import scala.reflect._
import scala.util._

trait Comparator[T] {
    def apply(a: T, b: T): Boolean
}

class IntComparator extends Comparator[Int] {
    override def apply(a: Int, b: Int): Boolean = a < b
}

object MergeSort { 
  final val seed = 42
  
  def mergeSortFast[T](ary: Array[T], comp: Comparator[T]): Array[T] = {
    def merge(a: Array[T], b: Array[T]): Array[T] = {
      val res = new Array[Any](a.length + b.length).asInstanceOf[Array[T]]
      var ai = 0
      var bi = 0
      while (ai < a.length && bi < b.length) {
        if (comp(a(ai), b(bi))) {
          res(ai + bi) = a(ai)
          ai += 1
        } else {
          res(ai + bi) = b(bi)
          bi += 1
        }
      }
      while (ai < a.length) {
          res(ai + bi) = a(ai)
          ai += 1
      }
      while (bi < b.length) {
          res(ai + bi) = b(bi)
          bi += 1
      }
      res
    }
    val len = ary.length
    if (len <= 1) ary
    else {
      val mid = len / 2
      val a = new Array[Any](mid).asInstanceOf[Array[T]]
      val b = new Array[Any](len - mid).asInstanceOf[Array[T]]
      
      for (i <- 0 until mid) a(i) = ary(i)
      for (i <- mid until len) b(i - mid) = ary(i)
      
      merge(mergeSortFast(a, comp), mergeSortFast(b, comp))
    }
  }
  
  def mergeSortGen[T](ary: Array[T], comp: Comparator[T]): Array[T] = {
    def merge(a: Array[T], b: Array[T]): Array[T] = {
      val res = new Array[Any](a.length + b.length)
      var ai = 0
      var bi = 0
      while (ai < a.length && bi < b.length) {
        if (comp(a(ai), b(bi))) {
          res(ai + bi) = a(ai)
          ai += 1
        } else {
          res(ai + bi) = b(bi)
          bi += 1
        }
      }
      while (ai < a.length) {
        res(ai + bi) = a(ai)
        ai += 1
      }
      while (bi < b.length) {
        res(ai + bi) = b(bi)
        bi += 1
      }
      res.asInstanceOf[Array[T]]
    }
    val len = ary.length
    if (len <= 1) ary
    else {
      val mid = len / 2
      val a = new Array[Any](mid)
      val b = new Array[Any](len - mid)

      for (i <- 0 until mid) a(i) = ary(i)
      for (i <- mid until len) b(i - mid) = ary(i)

      merge(mergeSortGen(a.asInstanceOf[Array[T]], comp), mergeSortGen(b.asInstanceOf[Array[T]], comp))
    }
  }

  def mergeSortCT[T: ClassTag](ary: Array[T], comp: Comparator[T]): Array[T] = {
    def merge(a: Array[T], b: Array[T]): Array[T] = {
      val res = new Array[T](a.length + b.length)
      var ai = 0
      var bi = 0
      while (ai < a.length && bi < b.length) {
        if (comp(a(ai), b(bi))) {
          res(ai + bi) = a(ai)
          ai += 1
        } else {
          res(ai + bi) = b(bi)
          bi += 1
        }
      }
      while (ai < a.length) {
        res(ai + bi) = a(ai)
        ai += 1
      }
      while (bi < b.length) {
        res(ai + bi) = b(bi)
        bi += 1
      }
      res
    }
    val len = ary.length
    if (len <= 1) ary
    else {
      val mid = len / 2
      val a = new Array[T](mid)
      val b = new Array[T](len - mid)

      for (i <- 0 until mid) a(i) = ary(i)
      for (i <- mid until len) b(i - mid) = ary(i)

      merge(mergeSortCT(a, comp), mergeSortCT(b, comp))
    }
  }
  
  def randomArray(len: Int) = {
    val ary = new Array[Int](len)
    val rnd = new Random(seed)
    for (i <- 0 until len) {
      ary(i) = rnd.nextInt(len)
    }
    ary
  }
  
  def main(args: Array[String]): Unit = {
    val lens = List(500000, 1000000, 3000000)
    
    for (len <- lens) {
      println("\nWith length : " + len + "\n")
      
      val aryA = randomArray(len)
      val startA = System.nanoTime
      mergeSortFast(aryA, new IntComparator)
      println("Array[Int] : " + (System.nanoTime - startA) / 1000000.0 + " milliseconds")

      val aryB = randomArray(len)
      val startB = System.nanoTime
      mergeSortGen(aryB, new IntComparator)
      println("Array[Any] : " + (System.nanoTime - startB) / 1000000.0 + " milliseconds")

      val aryC = randomArray(len)
      val startC = System.nanoTime
      mergeSortCT(aryC, new IntComparator)
      println("Array[T: ClassTag] : " + (System.nanoTime - startC) / 1000000.0 + " milliseconds")
    }
  }
}
