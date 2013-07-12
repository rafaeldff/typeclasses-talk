package comparing

object s01 {
  
  case class Dinheiro(val valor:Double) extends java.lang.Comparable[Dinheiro] {
    def compareTo(outro:Dinheiro):Int =
      this.valor compareTo outro.valor
  }
  
  def sort[T <: Comparable[T]](list: List[T]):List[T] = 
    list match {
      case _ :: Nil | Nil => list
      case x :: xs =>
        val (smaller, larger) = xs.partition(_.compareTo(x) < 0)
        sort(smaller) ::: x :: sort(larger)
    }
  
  sort( List(new Dinheiro(3), new Dinheiro(2), new Dinheiro(1)) )
  
}