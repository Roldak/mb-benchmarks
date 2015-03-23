package miniboxing.example

import MbArray._
import scala.reflect._
import scala.util._

trait Comparator[@miniboxed T] {
	def apply(a: T, b: T): Boolean
}

class IntComparator extends Comparator[Int] {
	override def apply(a: Int, b: Int): Boolean = a < b
}

object MergeSort { 
  final val seed = 42
  
  def mergeSortMB[@miniboxed T](ary: MbArray[T], comp: Comparator[T]): MbArray[T] = {
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
	  
	  for (i <- 0 until mid) a(i) = ary(i)
	  for (i <- mid until len) b(i - mid) = ary(i)
	  
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
	  
	  var aryD = randomArrayMB(len)
	  val startD = System.nanoTime
	  mergeSortMB(aryD, new IntComparator)
	  println("Array[@miniboxed T] : " + (System.nanoTime - startD) / 1000000.0 + " milliseconds")
	}
  }
}
