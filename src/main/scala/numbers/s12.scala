package numbers
import scala.math._

object s12 {
  case class Dinheiro(valor:Double)
  
  def sum(numeros: List[Double]) = 
    numeros.reduce(_ + _)
  
  def average(numeros: List[Double]) = 
    sum(numeros) / numeros.size
    
  def stdDev(numeros: List[Double]) = 
    sqrt( average( numeros.map{n => pow(n - average(numeros), 2)}) )
  
  val doubles = List(54.37, 22.43, 42.57)
  average(doubles)
  
  val dinheiros = List(Dinheiro(42.00), Dinheiro(31.50))
  //! average(dinheiros)
}
