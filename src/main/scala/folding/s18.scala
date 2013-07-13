package folding

object NumTypeclass {
  trait Num[N] {
    def plus(a: N, b: N): N
    def minus(a: N, b: N): N
    def times(a: N, b: N): N
    def div(a: N, b: N): N
    def fromInt(i: Int): N
    def lt(a: N, b: N): Boolean
    def squareRoot(n: N): N;
    def negate(n:N):N = times(fromInt(-1), n)
    
    class Ops(a:N) {
      def +(b:N):N       = plus(a,b)
      def -(b:N):N       = minus(a,b)
      def *(b:N):N       = times(a,b)
      def /(b:N):N       = div(a,b)
      def <(b:N):Boolean = lt(a,b)
      def sqrt:N         = squareRoot(a)
      def neg:N          = negate(a)
    }
  }
  
  implicit def enableInfix[N](n:N)(implicit num: Num[N]): Num[N]#Ops = new num.Ops(n)
  implicit class ConvertsToNum(val i:Int) extends AnyVal {
    def asNum[N:Num] = implicitly[Num[N]].fromInt(i)
  }
}

object s18 {
  import NumTypeclass._
  
  def sum[N: Num](nums: List[N]) =
    nums.foldRight(0.asNum)(_ + _)

}
