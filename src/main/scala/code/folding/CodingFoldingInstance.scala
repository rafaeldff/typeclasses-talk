package code
package folding

import my.FoldingTypeclass._
import Code._

object CodingFoldingInstance {
  
  implicit val numExpressionsAreFoldable = new Foldable[FoldableCode, Code] {
    def foldRight(l: Code)(t: Code)(f: (Code, Code) => Code): Code = {
      val varElement = freshVar
      val varResult = freshVar
      Code(s"""
      var ${varResult.e} = ${t.e};
      
      for (i in ${l.e}) {
        ${varElement.e} = ${l.e}[i];
        ${varResult.e} = ${f(varElement, varResult).e};
      }
      
      return ${varResult.e};
      """, l.vars ++ t.vars)
    }
  }

}