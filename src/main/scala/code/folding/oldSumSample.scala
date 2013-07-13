package code
package folding

object oldSumSample {
  import my.NumTypeclass._
  import my.FoldingTypeclass._ 
  
  def sum[N: Num](nums: List[N]) =
    nums.foldRight(0.asNum)(_ + _)
    
}
