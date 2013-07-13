package folding

object s19 {
  import NumTypeclass._
  
  def sum[N: Num](nums: List[N]) =
    nums.foldRight(0.asNum)(_ + _)

}
