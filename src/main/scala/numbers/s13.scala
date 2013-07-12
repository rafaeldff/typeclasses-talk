package numbers
import my.math13._

object s13 {
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

  val doubles = List(54.37, 22.43, 42.57)
  //! average(doubles)

  val dinheiros = List(Dinheiro(42.00), Dinheiro(31.50))
  //! average(dinheiros)
}
// Typeclass