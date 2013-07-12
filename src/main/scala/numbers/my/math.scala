package numbers
package my

import scala.math.Numeric
import scala.math.Fractional

object math {
  import Fractional.Implicits._
  import Ordering.Implicits._

  def pow[N: Fractional](n: N, e: Int): N =
    if (e == 0)
      implicitly[Numeric[N]].one
    else
      n * pow(n, e - 1)

  
  def sqrt[N: Fractional](x: N): N = {
    val helper = implicitly[Fractional[N]]
    import helper.fromInt
    val precision = fromInt(1) / fromInt(10000)
    
    def squareRootIter(guess: N, x: N): N =
      if (goodEnough(guess))
        guess
      else
        squareRootIter(improveGuess(guess), x)

    def improveGuess(guess: N): N =
      average(guess, x / guess)

    def goodEnough(guess: N) =
      (square(guess) - x).abs < precision
    
     def average(x: N, y: N) : N =
        (x + y) / fromInt(2)
     
     def square(value : N): N = value * value

    squareRootIter(fromInt(1), x)
  }

}