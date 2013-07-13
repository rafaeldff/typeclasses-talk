package code

object s21 {
  trait Num[N] {
    def plus(a: N, b: N): N
    def minus(a: N, b: N): N
    def times(a: N, b: N): N
    def div(a: N, b: N): N
    def fromInt(i: Int): N
    def squareRoot(a:N):N
    def negate(a:N):N = times(fromInt(-1),a)
    
    class Ops(a:N) {
      def +(b:N):N       = plus(a,b)
      def -(b:N):N       = minus(a,b)
      def /(b:N):N       = div(a,b)
      def *(b:N):N       = times(a,b)
      def sqrt:N         = squareRoot(a)
      def neg:N          = negate(a) 
    }
  }
  
  implicit def enableInfix[N](n:N)(implicit num: Num[N]): Num[N]#Ops = new num.Ops(n)
  implicit class ConvertsToNum(val i:Int) extends AnyVal {
    def asNum[N:Num] = implicitly[Num[N]].fromInt(i)
  }
  
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
