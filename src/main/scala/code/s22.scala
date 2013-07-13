package code

object s22 {
  import my.NumTypeclass._
  
  case class Code(e:String, _vars:Seq[String]) {
    val vars = _vars.sorted.distinct
    
    def toFunction:String = s"""
      function(${vars.mkString(",")}) {
        return $e
      }
    """
  }
  
  var count = -1
  def freshVar = {
    count += 1
    val newVar = s"x$count" 
    Code(newVar, Seq(newVar))
  }
  
  implicit val codeIsANum = new Num[Code] {
    def plus(a: Code, b: Code): Code    = Code(s"(${a.e} + ${b.e})", a.vars ++ b.vars)
    def minus(a: Code, b: Code): Code   = Code(s"(${a.e} - ${b.e})", a.vars ++ b.vars)
    def times(a: Code, b: Code): Code   = Code(s"(${a.e} * ${b.e})", a.vars ++ b.vars)
    def div(a: Code, b: Code): Code     = Code(s"(${a.e} / ${b.e})", a.vars ++ b.vars)
    def fromInt(i: Int): Code           = Code(i.toString, Seq())
    def lt(a: Code, b: Code): Boolean   = false
    def squareRoot(a:Code): Code        = Code(s"Math.sqrt(${a.e})", a.vars)
  }      
  
  def root[N:Num](a:N, b:N, c:N):N = {
    val delta = (b*b - 4.asNum[N] * a * c) 
    (b.neg + delta.sqrt) / (2.asNum[N] * a)
  }
  
}
