package folding

object s21 {
  import NumTypeclass._
  
  trait Foldable[C]
  
  def sum[N: Num, F: Foldable](nums: F[N]) =
    nums.foldRight(0.asNum)(_ + _)

}
