package pl.umk.mat.stasiu88.nfserver

import scalaz._
import Scalaz._

case class RicherInt(i: Int){
  def times(f: =>Unit){
    (0 until i) foreach {_=> f}
  }
}
case class RicherLong(i: Long){
  def times(f: =>Unit){
    (0L until i) foreach {_=> f}
  }
}
object Utils {
  implicit def toRicherInt(i:Int) = RicherInt(i)
  
  implicit def toRicherLong(i:Long) = RicherLong(i)
  
  def ??? = throw new UnsupportedOperationException("Not implemented yet")
  
  def timing(f: => Unit){
    val t0 = System.currentTimeMillis()
    f
    val t1 = System.currentTimeMillis()
    println("Time: "+(t1-t0)+" ms")
  }
  
  def timed[A](f: => A):A={
    val t0 = System.currentTimeMillis()
    val result:A = f
    val t1 = System.currentTimeMillis()
    println("Time: "+(t1-t0)+" ms")
    result
  }

}