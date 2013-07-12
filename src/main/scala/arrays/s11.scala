package arrays

import scala.reflect.ClassTag

object s11 {
  def arrayWith[T: ClassTag](t:T): Array[T] = {
    Array(t)
  }
  
  val justFortyTwo:Array[Int] = arrayWith(42)
      
  def foo[T: ClassTag](i: Int, a: T, b: T): Array[T] = i match {
    case 1 => arrayWith(a)
    case 2 => arrayWith(b)
    case 3 => Array()
  }
  
  assert( foo(1,"a","b") == Array("a") ) 
  
}