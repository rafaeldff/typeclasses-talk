package comparing

object s08 {
  import java.util.Comparator
  import LibDinheiro2._

  def sort[T](list: List[T])(implicit comparator: Comparator[T]): List[T] = list.sorted

  implicit val intsComparator = new Comparator[Int] {
    def compare(a: Int, b: Int) = a compare b
  }

  sort(3 :: 2 :: 1 :: Nil)

  
  
  
  
  
  
  
  
  
  
  
  
  // sort( (1, Dinheiro(2)) :: (2, Dinheiro(1)) :: Nil )

}
