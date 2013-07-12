package comparing

object LibDinheiro2 {                     //nosso código, em outro arquivo
  import java.util.Comparator
  
  case class Dinheiro(val centavos: Int)
  object Dinheiro {
    implicit val comparaDinheiro = new Comparator[Dinheiro] {
      def compare(a: Dinheiro, b: Dinheiro) =
        b.centavos - a.centavos
    }
  }
}

object s07 {
  import java.util.Comparator
  import LibDinheiro2.Dinheiro

  
  
  def sort[T](list: List[T])(implicit comparator: Comparator[T]): List[T] = 
    list match {
      case _ :: Nil | Nil => list
      case x :: xs =>
        val (smaller, larger) = xs.partition(comparator.compare(x, _) < 0)
        sort(smaller)(comparator) ::: x :: sort(larger)(comparator)
    }

  sort( List(Dinheiro(3), Dinheiro(2), Dinheiro(1)) )

}
