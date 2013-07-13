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
  
  trait Foldable[C[_],T,N] {
    def foldRight(l: C[T])(t:T)(f: (T,T)=>T):T
    
    def size(l:C[T])(implicit ev: Num[N]):N
  }
  
  implicit def listsAreFoldable[T,N] = new Foldable[List,T,N] {
    def foldRight(l: List[T])(t:T)(f: (T,T)=>T):T =
     l.foldRight(t)(f)
     
    def size(l: List[T])(implicit ev: Num[N]):N = l.size.asNum[N]
  }                                               //> listsAreFoldable: [T, N]=> tesxprts.Foldable[List,T,N]
  
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
  def freshFoldable: FoldableExpression[Expr] = fresh
                                                  //> freshFoldable: => tesxprts.FoldableExpression[tesxprts.Expr]
  
  implicit val expressionsAreNums = new Num[Expr] {
    def plus(a: Expr, b: Expr): Expr    = Expr(s"(${a.e} + ${b.e})", a.vars ++ b.vars)
    def minus(a: Expr, b: Expr): Expr   = Expr(s"(${a.e} - ${b.e})", a.vars ++ b.vars)
    def times(a: Expr, b: Expr): Expr   = Expr(s"(${a.e} * ${b.e})", a.vars ++ b.vars)
    def div(a: Expr, b: Expr): Expr     = Expr(s"(${a.e} / ${b.e})", a.vars ++ b.vars)
    def fromInt(i: Int): Expr           = Expr(i.toString)
    def lt(a: Expr, b: Expr): Boolean   = false
    def squareRoot(a:Expr): Expr        = Expr(s"sqrt(${a.e})", a.vars)
    override def negate(a:Expr): Expr            = Expr(s"-${a.e}", a.vars)
  }                                               //> expressionsAreNums  : tesxprts.Num[tesxprts.Expr] = tesxprts$$anonfun$main$
                                                  //| 1$$anon$2@252119
                                                  
  type FoldableExpression[T] = Expr
  
  implicit val numExpressionsAreFoldable = new Foldable[FoldableExpression,Expr,Expr] {
    def foldRight(l: Expr)(t:Expr)(f: (Expr,Expr)=>Expr):Expr ={
      val arg1 = fresh
      val arg2 = fresh
      Expr(s"${l.e}.fold(${t.e})(function(${arg1.e},${arg2.e}) {${f(arg1,arg2).e})")
    }
    
    def size(l:Expr)(implicit ev: Num[Expr]):Expr = Expr(s"${l.e}.length")
  }                                               //> numExpressionsAreFoldable  : tesxprts.Foldable[tesxprts.FoldableExpression,
                                                  //| tesxprts.Expr,tesxprts.Expr] = tesxprts$$anonfun$main$1$$anon$3@42e6bc11
  
  def aPlusB[N:Num](a:N,b:N) = a + b              //> aPlusB: [N](a: N, b: N)(implicit evidence$2: tesxprts.Num[N])N
  
  aPlusB(fresh, fresh); reset                     //> res0: tesxprts.Expr = Expr((a + b),Set(a, b))
  
  def roots[N:Num](a:N, b:N, c:N):N = {
    val delta:N = (b*b - 4.asNum[N] * a * c)
    (b.neg + delta.sqrt) / (2.asNum[N] * a)
  }                                               //> roots: [N](a: N, b: N, c: N)(implicit evidence$3: tesxprts.Num[N])N
 
  //roots(fresh, fresh, fresh).toFunction
  def sum[N,L[_],I](l:L[N])(implicit foldable: Foldable[L,N,I], num: Num[N]):N =
    foldable.foldRight(l)(0.asNum[N])(_ + _)      //> sum: [N, L[_], I](l: L[N])(implicit foldable: tesxprts.Foldable[L,N,I], imp
                                                  //| licit num: tesxprts.Num[N])N
 
  def average[N,L[_]](l:L[N])(implicit foldable: Foldable[L,N,N], num: Num[N]):N =
    sum(l) / foldable.size(l)                     //> average: [N, L[_]](l: L[N])(implicit foldable: tesxprts.Foldable[L,N,N], im
                                                  //| plicit num: tesxprts.Num[N])N
  
  
  sum(freshFoldable).toFunction; reset            //> res1: String = function() {
                                                  //|       a.fold(0)(function(b,c) {(b + c))
                                                  //|     }
  
  average(freshFoldable)                          //> res2: tesxprts.Expr = Expr((a.fold(0)(function(b,c) {(b + c)) / a.length),S
                                                  //| et())
  
  implicit val doublesAreNums = new Num[Double] {
      def plus(a: Double, b: Double): Double = a + b
      def minus(a: Double, b: Double): Double = a - b
      def times(a: Double, b: Double): Double = a * b
      def div(a: Double, b: Double): Double = a / b
      def fromInt(i: Int): Double = i.toDouble
      def lt(a: Double, b: Double): Boolean = a < b
      def squareRoot(n: Double): Double = scala.math.sqrt(n)
    }                                             //> doublesAreNums  : tesxprts.Num[Double] = tesxprts$$anonfun$main$1$$anon$4@a
                                                  //| f66eac
  
  sum(List(1.0,2.0,10.0))                         //> res3: Double = 13.0
  
  average(List(1.0,2.0,10.0))                     //> res4: Double = 4.333333333333333
                                                    
}