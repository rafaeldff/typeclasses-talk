object numbers {

  object Use {
	  import Numeric.Implicits._
	  
	  def baskara[T: Numeric](a:T, b:T, c:T) = {
	  	val numeric = implicitly[Numeric[T]]
	    (b*b) - numeric.fromInt(4) * a*c
	  }
	  
  }
  
  Use.baskara(3, 4, 5)                            //> res0: Int = -44
  
  Use.baskara(3.0, 4.0, 5.0)                      //> res1: Double = -44.0
  
  Use.baskara(BigInt(3), BigInt(4), BigInt(5))    //> res2: scala.math.BigInt = -44
  
  implicit val stringNumeric = new Numeric[String] {
	  def plus(x: String, y: String): String  = s"($x+$y)"
	  def minus(x: String, y: String): String = s"($x-$y)"
	  def times(x: String, y: String): String = s"($x*$y)"
	  def negate(x: String): String						= s"-$x"
	  def fromInt(x: Int): String							= x.toString
	  def toInt(x: String): Int								= ???
	  def toLong(x: String): Long							= ???
	  def toFloat(x: String): Float						= ???
	  def toDouble(x: String): Double					= ???
	  def compare(a:String,b:String) 					= ???
  }                                               //> stringNumeric  : Numeric[String]{def compare(a: String,b: String): Nothing} 
                                                  //| = numbers$$anonfun$main$1$$anon$1@1c398896
  Use.baskara("a", "b", "c")                      //> res3: String = ((b*b)-((4*a)*c))
}