package code

object s20 {
  import my.NumTypeclass._
  
  case class Code(e:String)
  
  implicit val codeIsANum = new Num[Code] {
    def plus(a: Code, b: Code): Code    = ???
    def minus(a: Code, b: Code): Code   = ???
    def times(a: Code, b: Code): Code   = ???
    def div(a: Code, b: Code): Code     = ???
    def fromInt(i: Int): Code           = ???
    def lt(a: Code, b: Code): Boolean   = ???
    def squareRoot(a:Code): Code        = ???
  }      
  
  def root[N:Num](a:N, b:N, c:N):N = {
    val delta = (b*b - 4.asNum[N] * a * c)
    (b.neg + delta.sqrt) / (2.asNum[N] * a)
  }
}
