package comparing

object s08 {
  import java.util.Comparator

  def sort[T](list: List[T])(implicit comparator: Comparator[T]): List[T] = ???

  implicit val intsComparator = new Comparator[Int] {
    def compare(a: Int, b: Int) = a compare b
  }

  sort(3 :: 2 :: 1 :: Nil)

  implicit def tuplesComparator[T1, T2]
      (implicit comp1: Comparator[T1], comp2: Comparator[T2]) = {
        
    new Comparator[(T1, T2)] {
      def compare(x: (T1, T2), y: (T1, T2)): Int = {
        comp1.compare(x._1, y._1) match {
          case g if g > 0 => 1
          case l if l < 0 => -1
          case e => comp2.compare(x._2, y._2)
        }
      }
    }
    
  }
  
  import LibDinheiro2._

  sort((1, Dinheiro(2)) :: (2, Dinheiro(1)) :: Nil)

}
// Recursive typeclasses