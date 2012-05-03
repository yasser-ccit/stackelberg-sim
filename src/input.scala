object Problem {
	val r = 2.5
	val ymax = Array(2,1,1.5,1,2)
	val a = Array(1,4,1,3,1)
	var y:Array[Double] = new Array[Double](5)
	var M:Array[Int] = new Array[Int](5)
	
	val A0 = Array(1,1,0)
	val A1 = Array(0,0,1)
	val A2 = Array(1,0,0)
	val A3 = Array(0,1,0)
	val A4 = Array(1,0,1)
	val A = Array(A0, A1, A2, A3, A4)
	
	def g0(y:Double): Double = -4*y+9
	def g1(y:Double) = 4.0/y
	def g2(y:Double) = 1.5/y
	def g3(y:Double) = 3.0/y
	def g4(y:Double) = -2*y+5
	
	def l0(y:Double, m:Int) = if (m==0) a(0) else g0(y)
	def l1(y:Double, m:Int) = if (m==0) a(1) else g1(y)
	def l2(y:Double, m:Int) = if (m==0) a(2) else g2(y)
	def l3(y:Double, m:Int) = if (m==0) a(3) else g3(y)
	def l4(y:Double, m:Int) = if (m==0) a(4) else g4(y)
	
	val ya = new Array[Double](5) //for M=(1,0,1,1,0)
	ya(0) = 7./4
	ya(1) = r - ya(0)
	ya(3) = 1
	ya(2) = ya(0) - ya(3)
	ya(4) = r - ya(3)
	
	val ya_bis = new Array[Double](5) //for M=(1,0,1,1,0) also
	ya_bis(0) = (Math.sqrt(13)+4)/(Math.sqrt(13)+3)
	ya_bis(1) = r - ya_bis(0)
	ya_bis(3) = (3*Math.sqrt(13)+9)/4
	ya_bis(2) = ya_bis(0) - ya_bis(3)
	ya_bis(4) = r - ya_bis(3)
	
	val yb = new Array[Double](5) //for M=(0,0,1,1,0)
	yb(3) = 3./4
	yb(0) = yb(3)+0.5
	yb(1) = r - yb(0)
	yb(2) = yb(0) - yb(3)
	yb(4) = r - yb(3)
	
	val yc = new Array[Double](5) //for M=(1,0,0,1,0)
	yc(3) = 3./2
	yc(0) = 3./2
	yc(1) = r - yc(0)
	yc(2) = yc(0) - yc(3)
	yc(4) = r - yc(3)
	
	val yd = new Array[Double](5) //for M=(1,0,1,0,0)
	yd(0) = 7./4
	yd(3) = yd(0)-3./4
	yd(1) = r - yd(0)
	yd(2) = yd(0) - yd(3)
	yd(4) = r - yd(3)
	
	def testFeasible(yy: Array[Double]) = {
	  var b = false
	  for (i <- 0 until yy.length){
	    b = yy(i)>=0 && yy(i)<= ymax(i)
	    println("y("+i+")>=0 && y("+i+")<= ymax("+i+")="+ymax(i)+": "+b)
	  }
	  val x0 = yy(2)
	  val x1 = yy(3)
	  val x2 = yy(1)
	   b = yy(0) == x0 + x1
	   println("y(0) = x0 + x1 = "+yy(0)+": "+b)
	   b = yy(1) == x2
	   println("y(1) = x2: = "+yy(1)+": "+b)
	   b = yy(2) == x0
	   println("y(2) = x0: = "+yy(2)+": "+b)
	   b = yy(3) == x1
	   println("y(3) = x1: = "+yy(3)+": "+b)
	   b = yy(4) == x0 + x2
	   println("y(4) = x0 + x2: = "+yy(4)+": "+b)
	   b = x0 + x1 + x2 == r
	   println("x0 + x1 + x2 = r: = "+r+": "+b)
	}
	
	def testCost(yy: Array[Double],MM: Array[Int]) = {
	  val cost = new Array[Double](3)
	  cost(0) = l0(yy(0), MM(0))+l2(yy(2), MM(2)) + l4(yy(4), MM(4))
	  cost(1) = l0(yy(0), MM(0))+l3(yy(3), MM(3))
	  cost(2) = l1(yy(1), MM(1))+l4(yy(4), MM(4))
	  for (i <- 0 until cost.length) println("Cost(x"+i+") = " + cost(i))
	  val b = cost(0) == cost(1) && cost(0) == cost(2)
	  println("Equal costs: "+ b)
	}
	
	def test(yy: Array[Double],MM: Array[Int]) = {
	  testFeasible(yy) 
	  testCost(yy,MM)
	}
	
	
	def main(args:Array[String]) = {
	  y = Array(1.5,1,0.5,1,1.5)
	  //We impose M(1)=M(4)=0 (worst path in free flow)
	  test (ya, Array(1,0,1,1,0))
	  
	  test (ya_bis, Array(1,0,1,1,0))
	  
	  test (yb, Array(0,0,1,1,0))
	  
	  test (yc, Array(1,0,0,1,0))
	  
	  test (yd, Array(1,0,1,0,0))
	  
	  test (Array(7./4,3./4,3./4,1,3./2), Array(1,0,1,0,0))

	}
}

