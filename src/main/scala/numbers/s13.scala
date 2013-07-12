package numbers
import my.math._

object s13 {
  import comparing.LibDinheiro2._
  
  trait Fractional[N] {
    def plus(a:N, b:N):N
    def div(a:N, b:N):N
    def fromInt(i:Int):N
  }
  
  def sum[N](nums: List[N])(implicit fractional: Fractional[N]) =
     nums.reduce((a,b) => fractional.plus(a, b))
  
  def average[N](nums: List[N])(implicit fractional: Fractional[N]) = 
    fractional.div(sum(nums), fractional.fromInt(nums.size))
  
//  def stdDev[N](nums: List[N])(implicit fractional: Fractional[N]) = 
//    sqrt( average( nums.map{n => pow(fractional.minus(n, average(nums)), 2)}) )
  
  val doubles = List(54.37, 22.43, 42.57)
  //! average(doubles)
  
  val dinheiros = List(Dinheiro(42.00), Dinheiro(31.50))
  //! average(dinheiros)
}
//Doubles