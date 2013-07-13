package code

object s19 {
  import my.NumTypeclass._
  
  case class Code(e:String)
  
  
  
  
  
  
  
  
  
  
  
  def root[N:Num](a:N, b:N, c:N):N = {
    val delta = (b*b - 4.asNum[N] * a * c)
    (b.neg + delta.sqrt) / (2.asNum[N] * a)
  }
}
