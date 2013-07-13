package code
package folding

case class Code(e: String, _vars: Seq[String]) {
  val vars = _vars.sorted.distinct

  def toFunction: String = s"""
      function(${vars.mkString(",")}) {
        return $e
      }
    """
}

object Code {
  type FoldableCode[T] = Code 
  
  var count = -1
  def freshVar = {
    count += 1
    val newVar = s"x$count" 
    Code(newVar, Seq(newVar))
  }
  
  def freshFoldableVar[T]: FoldableCode[T] = freshVar
}
