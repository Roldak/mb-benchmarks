package miniboxing.example

import MbArray._
import scala.reflect._
import scala.util._

object MergeSort { 
  final val seed = 42
  
  def mergeSortFast(ary: Array[Int], comp: (Int, Int) => Boolean): Array[Int] = {
    def merge(a: Array[Int], b: Array[Int]): Array[Int] = {
      val res = new Array[Int](a.length + b.length)
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
      val a = new Array[Int](mid)
      val b = new Array[Int](len - mid)

      var i = 0
	  while (i < mid) {
	    a(i) = ary(i)
	    i += 1
	  }
	  while (i < len) {
	    b(i - mid) = ary(i)
		i += 1
	  }

      merge(mergeSortFast(a, comp), mergeSortFast(b, comp))
    }
  }

  def mergeSortGen[T](ary: Array[T], comp: (T, T) => Boolean): Array[T] = {
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

      var i = 0
	  while (i < mid) {
	    a(i) = ary(i)
	    i += 1
	  }
	  while (i < len) {
	    b(i - mid) = ary(i)
		i += 1
	  }

      merge(mergeSortGen(a.asInstanceOf[Array[T]], comp), mergeSortGen(b.asInstanceOf[Array[T]], comp))
    }
  }

  def mergeSortCT[@miniboxed T: ClassTag](ary: Array[T], comp: (T, T) => Boolean): Array[T] = {
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

      var i = 0
	  while (i < mid) {
	    a(i) = ary(i)
	    i += 1
	  }
	  while (i < len) {
	    b(i - mid) = ary(i)
		i += 1
	  }

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
  
  def mergeSortMB[@miniboxed T](ary: MbArray[T], comp: (T, T) => Boolean): MbArray[T] = {
    def merge(a: MbArray[T], b: MbArray[T]): MbArray[T] = {
      val res = MbArray.empty[T](a.length + b.length)
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
      val a = MbArray.empty[T](mid)
      val b = MbArray.empty[T](len - mid)
      
      var i = 0
      while (i < mid) {
        a(i) = ary(i)
        i += 1
      }
      while (i < len) {
        b(i - mid) = ary(i)
        i += 1
      }
      
      merge(mergeSortMB(a, comp), mergeSortMB(b, comp))
    }
  }
  
  def randomArrayMB(len: Int) = {
    val ary = MbArray.empty[Int](len)
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
      var aryA = randomArray(len)
      val startA = System.nanoTime
      mergeSortFast(aryA, (a: Int, b: Int) => a < b)
      println("Array[Int] : " + (System.nanoTime - startA) / 1000000.0 + " milliseconds")
	  
	  aryA = null
	  System.gc()

      var aryB = randomArray(len)
      val startB = System.nanoTime
      mergeSortGen(aryB, (a: Int, b: Int) => a < b)
      println("Array[Any] : " + (System.nanoTime - startB) / 1000000.0 + " milliseconds")

	  aryB = null
	  System.gc()
	  
      var aryC = randomArray(len)
      val startC = System.nanoTime
      mergeSortCT(aryC, (a: Int, b: Int) => a < b)
      println("Array[T: ClassTag] : " + (System.nanoTime - startC) / 1000000.0 + " milliseconds")
      
	  aryC = null
	  System.gc()
	  
      var aryD = randomArrayMB(len)
      val startD = System.nanoTime
      mergeSortMB(aryD, (a: Int, b: Int) => a < b)
      println("Array[@miniboxed T] : " + (System.nanoTime - startD) / 1000000.0 + " milliseconds")
    }
  }
}
