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
  
  def mergeSort[T](ary: Array[T], comp: Comparator[T]): Array[T] = {
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
	  
	  merge(mergeSort(a, comp), mergeSort(b, comp))
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
	  
	  var aryD = randomArray(len)
	  val startD = System.nanoTime
	  mergeSort(aryD, new IntComparator)
	  println("Array[@miniboxed T] : " + (System.nanoTime - startD) / 1000000.0 + " milliseconds")
	}
  }
}
