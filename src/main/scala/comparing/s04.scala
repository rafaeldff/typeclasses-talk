package comparing

object s04 {
  import java.util.Collections
  import java.util.Comparator

  import TimeLib.Timestamp

  val comparaTimestamps = new Comparator[Timestamp] {
    def compare(a: Timestamp, b: Timestamp) =
      b.millis - a.millis
  }

  def sort[T](list: List[T])(comparator: Comparator[T]): List[T] =
    list match {
      case _ :: Nil | Nil => list
      case x :: xs =>
        val (smaller, larger) = xs.partition(comparator.compare(x, _) < 0)
        sort(smaller)(comparator) ::: x :: sort(larger)(comparator)
    }

  sort( List(Timestamp.yesterday, Timestamp.today) )(comparaTimestamps)
  
  sort( List(Timestamp.yesterday, Timestamp.today) )(Collections.reverseOrder(comparaTimestamps))

}
// Comparator offer support for 3d party libs