package code
package folding

import my.FoldingTypeclass._

object ListsFoldingInstance {
  
  
  implicit def listsAreFoldable[T] = new Foldable[List, T] {
    def foldRight(list: List[T])(t: T)(f: (T, T) => T): T =
      list.foldRight(t)(f)
  }
  

}