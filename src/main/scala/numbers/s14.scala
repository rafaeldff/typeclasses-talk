package numbers
import my.math14._

object s14 {
  case class Dinheiro(valor:Double)
  
  trait Num[N] {
    def plus(a: N, b: N): N
    def minus(a: N, b: N): N
    def times(a: N, b: N): N
    def div(a: N, b: N): N
    def fromInt(i: Int): N
    def lt(a: N, b: N): Boolean
  }

  object Num {
    implicit val doublesAreNums = new Num[Double] {
      def plus(a: Double, b: Double):  Double  = a + b
      def minus(a: Double, b: Double): Double  = a - b
      def times(a: Double, b: Double): Double  = a * b
      def div(a: Double, b: Double):   Double  = a / b
      def fromInt(i: Int):             Double  = i.toDouble
      def lt(a: Double, b: Double):    Boolean = a < b
    }
  }

  import comparing.LibDinheiro2._

  def sum[N](nums: List[N])(implicit fractional: Num[N]) =
    nums.reduce((a, b) => fractional.plus(a, b))

  def average[N](nums: List[N])(implicit fractional: Num[N]) =
    fractional.div(sum(nums), fractional.fromInt(nums.size))

  def stdDev[N](nums: List[N])(implicit fractional: Num[N]) =
    sqrt(average(nums.map { n => pow(fractional.minus(n, average(nums)), 2) }))

  val doubles = List(54.37, 22.43, 42.57)
  average(doubles)

  val dinheiros = List(Dinheiro(42.00), Dinheiro(31.50))
  //! average(dinheiros)
}
//TC impl for doubles