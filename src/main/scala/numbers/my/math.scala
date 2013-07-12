package numbers
package my

import scala.Ordering.Implicits.infixOrderingOps
import scala.math.Fractional
import scala.math.Fractional.Implicits.infixFractionalOps
import scala.math.Numeric

object math {
  import Fractional.Implicits._
  import Ordering.Implicits._

  def pow[N: Fractional](n: N, e: Int): N =
    if (e == 0)
      implicitly[Numeric[N]].fromInt(1)
    else
      n * pow(n, e - 1)
  
  def sqrt[N: Fractional](x: N): N = {
    val helper = implicitly[Fractional[N]]
    val ord = implicitly[Ordering[N]]
    import helper.fromInt
    val precision = helper.div(fromInt(1), fromInt(10000))
    
    def squareRootIter(guess: N, x: N): N =
      if (goodEnough(guess))
        guess
      else
        squareRootIter(improveGuess(guess), x)

    def improveGuess(guess: N): N =
      average(guess, helper.div(x, guess))

    def goodEnough(guess: N) =
      ord.lt(helper.abs(helper.minus(square(guess), x)),  precision)
    
     def average(x: N, y: N) : N =
        helper.div(helper.plus(x, y), fromInt(2))
     
     def square(value : N): N = helper.times(value, value)

    squareRootIter(fromInt(1), x)
  }

}