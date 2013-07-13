package code
package folding

object sumSample {
  import my.NumTypeclass._
  import my.FoldingTypeclass._ 
  
  def sum[N: Num,L[_]](nums: L[N])(implicit foldable: Foldable[L,N]) =
    foldable.foldRight(nums)(0.asNum)(_ + _)

}
