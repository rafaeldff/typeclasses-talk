package folding

object s21 {
  import NumTypeclass._
  
  trait Foldable[L[_],T] {
    def foldRight(l: L[T])(t:T)(f: (T,T)=>T):T
  }
  
  def sum[N: Num,L[_]](nums: L[N])(implicit foldable: Foldable[L,N]) =
    foldable.foldRight(nums)(0.asNum)(_ + _)

}
