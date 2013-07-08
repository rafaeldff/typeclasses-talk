object validation {
	
	trait ValidNum[T] {
	  type Result
	  def gt(a:T, b:T):Result
	  def fromInt(i:Int):T
	}
	
	case class Js(expr:String)
	
	
	object ValidNum {
	  implicit val intIsValid = new ValidNum[Int] {
	    type Result = Boolean
	    def gt(a:Int,b:Int) = a > b
	    def fromInt(i:Int) = i
	  }
	 
	  implicit val jsIsValid = new ValidNum[Js] {
	    type Result = Js
	    def gt(a:Js,b:Js) = Js(s"${a.expr} > ${b.expr}")
	    def fromInt(i:Int) = Js(i.toString)
	  }
	  
	}
	
	
	
	def fn[T](t:T)(implicit ev: ValidNum[T]): ev.Result = {
	  ev.gt(t, ev.fromInt(0))
	}                                         //> fn: [T](t: T)(implicit ev: validation.ValidNum[T])ev.Result
	
	
	fn(-1)                                    //> res0: validation.ValidNum.intIsValid.Result = false
	fn(10)                                    //> res1: validation.ValidNum.intIsValid.Result = true
	
	
	fn(Js("x"))                               //> res2: validation.ValidNum.jsIsValid.Result = Js(x > 0)
	
	
}