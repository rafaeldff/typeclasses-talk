package comparing

object TimeLib {
  case class Timestamp(val millis:Int)
  object Timestamp {
    def today = new Timestamp(0)
    def yesterday = new Timestamp(-1)
  }
}

object s02 {
  
  import TimeLib.Timestamp
  
  def sort[T <: Comparable[T]](list: List[T]):List[T] = 
    list match {
      case _ :: Nil | Nil => list
      case x :: xs =>
        val (smaller, larger) = xs.partition(_.compareTo(x) < 0)
        sort(smaller) ::: x :: sort(larger)
    }
  
   // sort( List(Timestamp.today, Timestamp.yesterday) )  
  
}
// Comparable offers no support for 3rd party libs 