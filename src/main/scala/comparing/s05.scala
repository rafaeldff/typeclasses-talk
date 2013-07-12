package comparing

object s05 {
  
  import java.util.Comparator

  import TimeLib.Timestamp

  implicit val comparaTimestamps = new Comparator[Timestamp] {
    def compare(a: Timestamp, b: Timestamp) =
      b.millis - a.millis
  }

  def sort[T](list: List[T])(implicit comparator: Comparator[T]): List[T] =
    list match {
      case _ :: Nil | Nil => list
      case x :: xs =>
        val (smaller, larger) = xs.partition(comparator.compare(x, _) < 0)
        sort(smaller)(comparator) ::: x :: sort(larger)(comparator)
    }

  sort( List(Timestamp.yesterday, Timestamp.today) )
  
  
}
