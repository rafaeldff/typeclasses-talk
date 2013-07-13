package code.my

object FoldingTypeclass {
  
  
  trait Foldable[L[_], T] {
    def foldRight(l: L[T])(t: T)(f: (T, T) => T): T
  }
  
  
}
