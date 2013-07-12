package comparing

object s03 {
  import java.util.Comparator
  
  case class Dinheiro(val valor:Double)  
  
  val comparaDinheiro = new Comparator[Dinheiro] {
    def compare(a:Dinheiro, b:Dinheiro) =
      (b.valor - a.valor).toInt
  }
  
  def sort[T](list: List[T])(comparator: Comparator[T]):List[T] = 
    list match {
      case _ :: Nil | Nil => list
      case x :: xs =>
        val (smaller, larger) = xs.partition(comparator.compare(x, _) < 0)
        sort(smaller)(comparator) ::: x :: sort(larger)(comparator)
    }
  
  sort( List(Dinheiro(3), Dinheiro(2), Dinheiro(1)) )(comparaDinheiro)
  
}