import scala.collection.immutable.HashSet

object noavg {
 
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
                                                  //> enableInfix: [N](n: N)(implicit num: noavg.Num[N])noavg.Num[N]#Ops
  implicit class ConvertsToNum(val i:Int) {
    def asNum[N:Num] = implicitly[Num[N]].fromInt(i)
  }
  
  trait Foldable[C[_],T] {
    def foldRight(l: C[T])(t:T)(f: (T,T)=>T):T
  }
  
  implicit def listsAreFoldable[T] = new Foldable[List,T] {
    def foldRight(l: List[T])(t:T)(f: (T,T)=>T):T =
     l.foldRight(t)(f)
  }                                               //> listsAreFoldable: [T]=> noavg.Foldable[List,T]
  
  case class Expr(e:String, vars:Set[String]=HashSet(), valueIsStatement:Boolean=true) {
    def toFunction = s"""
    function(${vars.mkString(",")}) {
      $e
    }"""
        
  }
  var latest = -1                                 //> latest  : Int = -1
  def reset = {latest = -1}                       //> reset: => Unit
  val dict = 'a' to 'z'                           //> dict  : scala.collection.immutable.NumericRange.Inclusive[Char] = NumericRa
                                                  //| nge(a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x,
                                                  //|  y, z)
                                                  
  def fresh = {latest += 1; val newVar = dict(latest).toString; Expr(newVar, Set(newVar))}
                                                  //> fresh: => noavg.Expr
  def freshFoldable: FoldableExpression[Expr] = fresh
                                                  //> freshFoldable: => noavg.FoldableExpression[noavg.Expr]
  
  implicit val expressionsAreNums = new Num[Expr] {
    def plus(a: Expr, b: Expr): Expr    = Expr(s"(${a.e} + ${b.e})", a.vars ++ b.vars)
    def minus(a: Expr, b: Expr): Expr   = Expr(s"(${a.e} - ${b.e})", a.vars ++ b.vars)
    def times(a: Expr, b: Expr): Expr   = Expr(s"(${a.e} * ${b.e})", a.vars ++ b.vars)
    def div(a: Expr, b: Expr): Expr     = Expr(s"(${a.e} / ${b.e})", a.vars ++ b.vars)
    def fromInt(i: Int): Expr           = Expr(i.toString)
    def lt(a: Expr, b: Expr): Boolean   = false
    def squareRoot(a:Expr): Expr        = Expr(s"sqrt(${a.e})", a.vars)
    override def negate(a:Expr): Expr            = Expr(s"-${a.e}", a.vars)
  }                                               //> expressionsAreNums  : noavg.Num[noavg.Expr] = noavg$$anonfun$main$1$$anon$2
                                                  //| @31c74456
                                                  
  type FoldableExpression[T] = Expr
  
  implicit val numExpressionsAreFoldable = new Foldable[FoldableExpression,Expr] {
    def foldRight(l: Expr)(t:Expr)(f: (Expr,Expr)=>Expr):Expr ={
      val varElement = fresh
      val varResult = fresh
      Expr(s"""
      var ${varResult.e} = ${t.e};
      
      for (i in ${l.e}) {
        ${varElement.e} = ${l.e}[i];
        ${varResult.e} = ${f(varElement, varResult).e};
      }
      
      return ${varResult.e};
      """, l.vars ++ t.vars)
    }
  }                                               //> numExpressionsAreFoldable  : noavg.Foldable[noavg.FoldableExpression,noavg.
                                                  //| Expr] = noavg$$anonfun$main$1$$anon$3@5462a19d
  
  def aPlusB[N:Num](a:N,b:N) = a + b              //> aPlusB: [N](a: N, b: N)(implicit evidence$2: noavg.Num[N])N
  
  aPlusB(fresh, fresh).toFunction; reset          //> res0: String = "
                                                  //|     function(a,b) {
                                                  //|       (a + b)
                                                  //|     }"
  
  def roots[N:Num](a:N, b:N, c:N):N = {
    val delta:N = (b*b - 4.asNum[N] * a * c)
    (b.neg + delta.sqrt) / (2.asNum[N] * a)
  }                                               //> roots: [N](a: N, b: N, c: N)(implicit evidence$3: noavg.Num[N])N
 
  roots(fresh, fresh, fresh).toFunction           //> res1: String = "
                                                  //|     function(b,a,c) {
                                                  //|       ((-b + sqrt(((b * b) - ((4 * a) * c)))) / (2 * a))
                                                  //|     }"
  def sum[N: Num,L[_]](l:L[N])(implicit foldable: Foldable[L,N]):N =
    foldable.foldRight(l)(0.asNum[N])(_ + _)      //> sum: [N, L[_]](l: L[N])(implicit evidence$4: noavg.Num[N], implicit foldabl
                                                  //| e: noavg.Foldable[L,N])N
 
  
  println(sum(freshFoldable).toFunction); reset   //> 
                                                  //|     function(d) {
                                                  //|       
                                                  //|       var f = 0;
                                                  //|       
                                                  //|       for (i in d) {
                                                  //|         e = d[i];
                                                  //|         f = (e + f);
                                                  //|       }
                                                  //|       
                                                  //|       return f;
                                                  //|       
                                                  //|     }
  
  
  implicit val doublesAreNums = new Num[Double] {
      def plus(a: Double, b: Double): Double = a + b
      def minus(a: Double, b: Double): Double = a - b
      def times(a: Double, b: Double): Double = a * b
      def div(a: Double, b: Double): Double = a / b
      def fromInt(i: Int): Double = i.toDouble
      def lt(a: Double, b: Double): Boolean = a < b
      def squareRoot(n: Double): Double = scala.math.sqrt(n)
    }                                             //> doublesAreNums  : noavg.Num[Double] = noavg$$anonfun$main$1$$anon$4@26ae58e
                                                  //| 9
  
  sum(List(1.0,2.0,10.0))                         //> res2: Double = 13.0
  
}