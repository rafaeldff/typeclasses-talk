package numbers
import scala.math._

object s12 {
  case class Dinheiro(valor:Double)
  
  def sum(nums: List[Double]) = 
    nums.reduce(_ + _)
  
  def average(nums: List[Double]) = 
    sum(nums) / nums.size
    
  def stdDev(nums: List[Double]) = 
    sqrt( average( nums.map{n => pow(n - average(nums), 2)}) )
  
  val doubles = List(54.37, 22.43, 42.57)
  average(doubles)
  
  val dinheiros = List(Dinheiro(42.00), Dinheiro(31.50))
  //! average(dinheiros)
}
//Doubles (nao dinheiros)