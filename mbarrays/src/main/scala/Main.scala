package miniboxing.example

import MbArray._

object Test {
  final val len = 20000000
 
  def sumA(ary: Array[Any]) = {
    for (i <- 1 until len) {
        ary(i) = 1
        ary(0) = ary(0).asInstanceOf[Int] + ary(i).asInstanceOf[Int]
    }
    ary(0)
  }
 
  def sumB(ary: MbArray[Int]) = {
    for (i <- 1 until len) {
        ary(i) = 1
        ary(0) = ary(0) + ary(i)
    }
    ary(0)
  }

  def main(args: Array[String]): Unit = {
    val a = new Array[Any](len)
    val b = MbArray.empty[Int](len)
    
    println(a.getClass())
    for (i <- 0 until 30) {
        val startA = System.nanoTime
        val sA = sumA(a)
        println(sA + " in " + (System.nanoTime - startA) / 1000000.0 + " milliseconds")
    }
   
    println(b.getClass())
    for (i <- 0 until 30) {
        val startB = System.nanoTime
        val sB = sumB(b)
        println(sB + " in " + (System.nanoTime - startB) / 1000000.0 + " milliseconds")
    }
  }
}