package numbers
package my

import scala.math.Numeric

object math16 {
   import s16.Num
    
  def pow[N: Num](n: N, e: Int): N =
    if (e == 0)
      implicitly[Num[N]].fromInt(1)
    else
      implicitly[Num[N]].times(n, pow(n, e - 1))
  
  def sqrt[N: Num](x: N): N = {
    val helper = implicitly[Num[N]]
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
      helper.lt(abs(helper.minus(square(guess), x)),  precision)
    
     def average(x: N, y: N) : N =
        helper.div(helper.plus(x, y), fromInt(2))
     
     def square(value : N): N = helper.times(value, value)
     
     def abs(n:N):N =
       if (helper.lt(n, fromInt(0)))
         helper.times(n, fromInt(-1))
       else
         n

    squareRootIter(fromInt(1), x)
  }

}