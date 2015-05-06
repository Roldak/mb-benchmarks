package miniboxing.example
 
import MbArray._

object Test {
  final val len = 10000000
  final val steps = 30
  final def ONE_MILLION = 1000000.0
 
  def sumA(ary: Array[Any]) = {
    var i = 1
    while (i < len) {
      ary(i) = 1
      ary(0) = ary(0).asInstanceOf[Int] + ary(i).asInstanceOf[Int]
      i += 1
    }
    ary(0)
  }
 
  def sumB(ary: MbArray[Int]) = {
    var i = 1
    while (i < len) {
      ary(i) = 1
      ary(0) = ary(0) + ary(i)
      i += 1
    }
    ary(0)
  }

  def sumC(ary: Array[Int]) = {
    var i = 1
    while (i < len) {
      ary(i) = 1
      ary(0) = ary(0) + ary(i)
      i += 1
    }
    ary(0)
  }
 
 
  def main(args: Array[String]): Unit = {
    val a = new Array[Any](len)
    val b = MbArray.empty[Int](len)
    val c = new Array[Int](len)
    var ta = 0l
    var tb = 0l
    var tc = 0l
   
    println(a.getClass())
    for (i <- 0 until steps) {
      val startA = System.nanoTime
      val sA = sumA(a)
      println(sA + " in " + (System.nanoTime - startA) / ONE_MILLION + " milliseconds")
      ta += System.nanoTime - startA
    }
  
    println(b.getClass())
    for (i <- 0 until steps) {
      val startB = System.nanoTime
      val sB = sumB(b)
      println(sB + " in " + (System.nanoTime - startB) / ONE_MILLION + " milliseconds")
      tb += System.nanoTime - startB
    }
   
    println(c.getClass())
    for (i <- 0 until steps) {
      val startC = System.nanoTime
      val sC = sumC(c)
      println(sC + " in " + (System.nanoTime - startC) / ONE_MILLION + " milliseconds")
      tc += System.nanoTime - startC
    }
	
  	println("Averages : ")
  	println("Array[Any] : " + ta / ONE_MILLION / steps)
  	println("MbArray[Int] : " + tb / ONE_MILLION / steps)
  	println("Array[Int] : " + tc / ONE_MILLION / steps)

    assert(ta > tb, s"Array[Any] ($ta) is worse than MbArray[Int] ($tb)")
    assert(tb > tc, s"MbArray[Int] ($tb) is worse than Array[Int] ($tc)")
  }
}
