import scala.collection.immutable.HashSet

object tesxprts {
 
  trait Num[N] {
    def plus(a: N, b: N): N
    def minus(a: N, b: N): N
    def times(a: N, b: N): N
    def div(a: N, b: N): N
    def fromInt(i: Int): N
    def lt(a: N, b: N): Boolean
    def squareRoot(n: N): N;
    def negate(n:N):N = times(fromInt(-1), n)
    
    class Ops(a:N) {
      def +(b:N):N       = plus(a,b)
      def -(b:N):N       = minus(a,b)
      def *(b:N):N       = times(a,b)
      def /(b:N):N       = div(a,b)
      def <(b:N):Boolean = lt(a,b)
      def sqrt:N         = squareRoot(a)
      def neg:N          = negate(a)
    }
  }
  
  implicit def enableInfix[N](n:N)(implicit num: Num[N]): Num[N]#Ops = new num.Ops(n)
                                                  //> enableInfix: [N](n: N)(implicit num: tesxprts.Num[N])tesxprts.Num[N]#Ops
  implicit class ConvertsToNum(val i:Int) {
    def asNum[N:Num] = implicitly[Num[N]].fromInt(i)
  }
  
  trait Foldable[C,T] {
    def foldRight(l: C)(t:T)(f: (T,T)=>T):T
  }
  
  implicit def listsAreFoldable[T] = new Foldable[List[T],T] {
    def foldRight(l: List[T])(t:T)(f: (T,T)=>T):T =
     l.foldRight(t)(f)
  }                                               //> listsAreFoldable: [T]=> tesxprts.Foldable[List[T],T]
  
  case class Expr(e:String, vars:Set[String]=HashSet()) {
    def toFunction = s"""function(${vars.mkString(",")}) {
      $e
    }"""
  }
  var latest = -1                                 //> latest  : Int = -1
  def reset = {latest = -1}                       //> reset: => Unit
  val dict = 'a' to 'z'                           //> dict  : scala.collection.immutable.NumericRange.Inclusive[Char] = NumericRa
                                                  //| nge(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x,
                                                  //|  y, z)
                                                  
  def fresh = {latest += 1; val newVar = dict(latest).toString; Expr(newVar, Set(newVar))}
                                                  //> fresh: => tesxprts.Expr
  
  implicit val expressionsAreNums = new Num[Expr] {
      def plus(a: Expr, b: Expr): Expr    = Expr(s"(${a.e} + ${b.e})", a.vars ++ b.vars)
      def minus(a: Expr, b: Expr): Expr   = Expr(s"(${a.e} - ${b.e})", a.vars ++ b.vars)
      def times(a: Expr, b: Expr): Expr   = Expr(s"(${a.e} * ${b.e})", a.vars ++ b.vars)
      def div(a: Expr, b: Expr): Expr     = Expr(s"(${a.e} / ${b.e})", a.vars ++ b.vars)
      def fromInt(i: Int): Expr           = Expr(i.toString)
      def lt(a: Expr, b: Expr): Boolean   = false
      def squareRoot(a:Expr): Expr        = Expr(s"sqrt(${a.e})", a.vars)
      override def negate(a:Expr): Expr            = Expr(s"-${a.e}", a.vars)
    }                                             //> expressionsAreNums  : tesxprts.Num[tesxprts.Expr] = tesxprts$$anonfun$main$
                                                  //| 1$$anon$2@640edd19
                                                  
  case class ArrayExpr[T](e:String)
  def freshArray:ArrayExpr[Expr] = {latest += 1; val newVar = dict(latest).toString; ArrayExpr(newVar)}
                                                  //> freshArray: => tesxprts.ArrayExpr[tesxprts.Expr]
  implicit def numExpressionsAreFoldable[T: Num] = new Foldable[Expr,Expr] {
    def foldRight(l: Expr)(t:Expr)(f: (Expr,Expr)=>Expr):Expr ={
      val arg1 = fresh
      val arg2 = fresh
      Expr(s"${l.e}.fold(${t.e})(function(${arg1.e},${arg2.e}) {${f(arg1,arg2).e})")
    }
  }                                               //> numExpressionsAreFoldable: [T](implicit evidence$2: tesxprts.Num[T])tesxprt
                                                  //| s.Foldable[tesxprts.Expr,tesxprts.Expr]
  
  def aPlusB[N:Num](a:N,b:N) = a + b              //> aPlusB: [N](a: N, b: N)(implicit evidence$3: tesxprts.Num[N])N
  
  aPlusB(fresh, fresh)                            //> res0: tesxprts.Expr = Expr((a + b),Set(a, b))
  
  def roots[N:Num](a:N, b:N, c:N):N = {
    val delta:N = (b*b - 4.asNum[N] * a * c)
    (b.neg + delta.sqrt) / (2.asNum[N] * a)
  }                                               //> roots: [N](a: N, b: N, c: N)(implicit evidence$4: tesxprts.Num[N])N
 
  roots(fresh, fresh, fresh).toFunction           //> res1: String = function(d,e,c) {
                                                  //|       ((-d + sqrt(((d * d) - ((4 * c) * e)))) / (2 * c))
                                                  //|     }
  def sum[N:Num,L](l:L)(implicit foldable: Foldable[L,N]):N =
    foldable.foldRight(l)(0.asNum[N])(_ + _)      //> sum: [N, L](l: L)(implicit evidence$5: tesxprts.Num[N], implicit foldable: 
                                                  //| tesxprts.Foldable[L,N])N
 
  
  sum(fresh).toFunction                           //> res2: String = function() {
                                                  //|       f.fold(0)(function(g,h) {(g + h))
                                                  //|     }
  
                                                    
}