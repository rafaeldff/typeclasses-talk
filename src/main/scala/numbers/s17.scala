package numbers
import my.math17._

object s17 {
  case class Dinheiro(valor:Double)
  
  trait Num[N] {
    def plus(a: N, b: N): N
    def minus(a: N, b: N): N
    def times(a: N, b: N): N
    def div(a: N, b: N): N
    def fromInt(i: Int): N
    def lt(a: N, b: N): Boolean
    
    class Ops(a:N) {
      def +(b:N):N       = plus(a,b)
      def -(b:N):N       = minus(a,b)
      def /(b:N):N       = div(a,b)
      def <(b:N):Boolean = lt(a,b)
    }
  }
  
  implicit def enableInfix[N](n:N)(implicit num: Num[N]): Num[N]#Ops = new num.Ops(n)
  implicit class ConvertsToNum(val i:Int) extends AnyVal {
    def asNum[N:Num] = implicitly[Num[N]].fromInt(i)
  }

  def sum[N: Num](nums: List[N]) =
    nums.reduce(_ + _)

  def average[N: Num](nums: List[N]) =
    sum(nums) / nums.size.asNum

  def stdDev[N: Num](nums: List[N]) =
    sqrt(average(nums.map { n => pow((n - average(nums)), 2) }))

  object Num {
    implicit val dinheirosAreNums = new Num[Dinheiro] {
      def plus(a: Dinheiro, b: Dinheiro): Dinheiro = Dinheiro(a.valor + b.valor)
      def minus(a: Dinheiro, b: Dinheiro): Dinheiro = Dinheiro(a.valor - b.valor)
      def times(a: Dinheiro, b: Dinheiro): Dinheiro = Dinheiro(a.valor * b.valor)
      def div(a: Dinheiro, b: Dinheiro): Dinheiro = Dinheiro(a.valor / b.valor)
      def fromInt(i: Int): Dinheiro = Dinheiro(i.toDouble)
      def lt(a: Dinheiro, b: Dinheiro): Boolean = a.valor < b.valor
    }
  }
  
  val dinheiros = List(Dinheiro(42.00), Dinheiro(31.50))
  average(dinheiros)
}
//TC impl for dinheiros 