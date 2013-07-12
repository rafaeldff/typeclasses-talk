package comparing

object LibDinheiro {
  import java.util.Comparator
  
  case class Dinheiro(val centavos:Int)
  implicit val comparaDinheiro = new Comparator[Dinheiro] {
    def compare(a:Dinheiro, b:Dinheiro) =
      b.centavos - a.centavos
  }
}

object s06 {
  import java.util.Comparator
  import LibDinheiro.Dinheiro
  
  import LibDinheiro.comparaDinheiro
  
  def sort[T](list: List[T])(implicit comparator: Comparator[T]):List[T] = 
    list match {
      case _ :: Nil | Nil => list
      case x :: xs =>
        val (smaller, larger) = xs.partition(comparator.compare(x, _) < 0)
        sort(smaller)(comparator) ::: x :: sort(larger)(comparator)
    }
  
  sort( List(Dinheiro(3), Dinheiro(2), Dinheiro(1)) )
  
}
// Implicits para dinheiro. Precisa sempre importar o implicit val?