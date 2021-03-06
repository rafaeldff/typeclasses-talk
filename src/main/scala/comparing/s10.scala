package comparing

object s10 {
  import java.util.Comparator
  import LibDinheiro2._

  def sort[T](list: List[T])(implicit comparator: Comparator[T]): List[T] = list.sorted

  implicit val intsComparator = new Comparator[Int] {
    def compare(a: Int, b: Int) = a compare b
  }

  sort(3 :: 2 :: 1 :: Nil)

  implicit def 
  tuplesComparator[T1: Comparator, T2: Comparator] =
    new Comparator[(T1, T2)] {
      def compare(x: (T1, T2), y: (T1, T2)): Int = {
        implicitly[Comparator[T1]].compare(x._1, y._1) match {
          case g if g > 0 => g
          case l if l < 0 => l
          case e => implicitly[Comparator[T2]].compare(x._2, y._2)
        }
      }
    }

  sort( (1, Dinheiro(2)) :: (2, Dinheiro(1)) :: Nil )
}