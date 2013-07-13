package code

object s21 {
  import my.NumTypeclass._
  
  case class Code(e:String)
  
  implicit val codeIsANum = new Num[Code] {
    def plus(a: Code, b: Code): Code    = Code(s"(${a.e} + ${b.e})")
    def minus(a: Code, b: Code): Code   = Code(s"(${a.e} - ${b.e})")
    def times(a: Code, b: Code): Code   = Code(s"(${a.e} * ${b.e})")
    def div(a: Code, b: Code): Code     = Code(s"(${a.e} / ${b.e})")
    def fromInt(i: Int): Code           = Code(i.toString)
    def lt(a: Code, b: Code): Boolean   = false
    def squareRoot(a:Code): Code        = Code(s"sqrt(${a.e})")
  }      
  
  def root[N:Num](a:N, b:N, c:N):N = {
    val delta = (b*b - 4.asNum[N] * a * c)
    (b.neg + delta.sqrt) / (2.asNum[N] * a)
  }
  
}
