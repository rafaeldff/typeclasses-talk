package numbers
import my.math15._

object s15 {
  case class Dinheiro(valor:Double)
  
  trait Num[N] {
    def plus(a: N, b: N): N
    def minus(a: N, b: N): N
    def times(a: N, b: N): N
    def div(a: N, b: N): N
    def fromInt(i: Int): N
    def lt(a: N, b: N): Boolean
  }

  def sum[N](nums: List[N])(implicit num: Num[N]) =
    nums.reduce((a, b) => num.plus(a, b))

  def average[N](nums: List[N])(implicit num: Num[N]) =
    num.div(sum(nums), num.fromInt(nums.size))

  def stdDev[N](nums: List[N])(implicit num: Num[N]) =
    sqrt(average(nums.map { n => pow(num.minus(n, average(nums)), 2) }))

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