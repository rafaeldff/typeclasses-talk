object numbers {

  
  trait BoolLike[B] {
    def toBoolean(b:B):Boolean
    def fromBoolean(b:Boolean):B
  }
  
  object BoolLike {
    implicit val booleansAreBoolLike = new BoolLike[Boolean] {
      def toBoolean(b:Boolean) = b
      def fromBoolean(b:Boolean) = b
    }
  }
  
  
  object NumLike {
    object Implicits {
      implicit def infixOps[N](n:N)(implicit ev: NumLike[N]): ev.Ops = new ev.Ops(n)
    }
    implicit def intsAreNums = new NumLike[Int] {
	    def plus(x: Int, y: Int): Int = x + y
	  	def minus(x: Int, y: Int): Int = x - y
	  	def times(x: Int, y: Int): Int = x * y
	  	def div(x: Int, y: Int): Int = x / y
			def fromInt(x:Int): Int = x
			def lte[B: BoolLike](x:Int, y:Int): B = {
				implicitly[BoolLike[B]].fromBoolean(x <= y)
			}
    }
  }

  trait NumLike[T] {
    def plus(x: T, y: T): T
  	def minus(x: T, y: T): T
  	def times(x: T, y: T): T
  	def div(x: T, y: T): T
		def fromInt(x:Int): T
		def lte[B: BoolLike](x:T, y:T): B
		
		class Ops(x:T) {
		  def +(y:T):T = plus(x,y)
		  def -(y:T):T = minus(x,y)
		  def *(y:T):T = times(x,y)
		  def /(y:T):T = div(x,y)
		  def <=[B:BoolLike](y:T) = lte[B](x,y)
		}
  }


  object Use {
	  import NumLike.Implicits._
	  //import Ordering.Implicits._
	  
	  def baskara[T: NumLike](a:T, b:T, c:T) = {
	  	val numeric = implicitly[NumLike[T]]
	    (b*b) - numeric.fromInt(4) * a*c
	  }
	  
	  //case class Item(value: Int, product: String)
	  //case class Order(items:List[Int], total:Int)
	  //
	  //def orderIsValid(order:Order):Boolean =
	  //  order.items.sum == order.total
	  //
	  //def sumIsValid[N:Numeric](xs:List[N], total:N):Boolean = {
	  //  val numeric = implicitly[Numeric[N]]
	  //  //	    numeric.compare(xs.sum, total) == 0
	  //  xs.sum <= total
	  //}
	  
  }
  
  Use.baskara(3, 4, 5)
  
  //Use.baskara(3.0, 4.0, 5.0)
  
  //Use.baskara(BigInt(3), BigInt(4), BigInt(5))
  
  implicit val stringNumLike = new NumLike[String] {
	  def plus(x: String, y: String): String  = s"($x+$y)"
	  def minus(x: String, y: String): String = s"($x-$y)"
	  def times(x: String, y: String): String = s"($x*$y)"
	  def div(x: String, y:String): String		= s"($x/$y)"
	  def fromInt(x: Int): String							= x.toString
	  def lte[B: BoolLike](x:Int, y:Int): B = {
				implicitly[BoolLike[B]].fromBoolean(x <= y)
			}
  }
  
 // Use.baskara("a", "b", "c")
  
  
}