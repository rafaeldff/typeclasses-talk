package code
package folding

import my.NumTypeclass._

object CodingNumInstance {
  implicit val codeIsANum = new Num[Code] {
    def plus(a: Code, b: Code): Code = Code(s"(${a.e} + ${b.e})", a.vars ++ b.vars)
    def minus(a: Code, b: Code): Code = Code(s"(${a.e} - ${b.e})", a.vars ++ b.vars)
    def times(a: Code, b: Code): Code = Code(s"(${a.e} * ${b.e})", a.vars ++ b.vars)
    def div(a: Code, b: Code): Code = Code(s"(${a.e} / ${b.e})", a.vars ++ b.vars)
    def fromInt(i: Int): Code = Code(i.toString, Seq())
    def lt(a: Code, b: Code): Boolean = false
    def squareRoot(a: Code): Code = Code(s"Math.sqrt(${a.e})", a.vars)
  }
}