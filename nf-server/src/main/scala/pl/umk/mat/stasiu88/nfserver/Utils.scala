/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

import scalaz._
import Scalaz._

case class RicherInt(i: Int){
  /**
   * Repeats the given block of code <code>i</code> times.
   * <br>
   * Powtarza <code>i</code> razy dany blok kodu.
   */
  def times(f: =>Unit){
    (0 until i) foreach {_=> f}
  }
}
case class RicherLong(i: Long){
  /**
   * Repeats the given block of code <code>i</code> times.
   * <br>
   * Powtarza <code>i</code> razy dany blok kodu.
   */
  def times(f: =>Unit){
    (0L until i) foreach {_=> f}
  }
}
object Utils {
  implicit def toRicherInt(i:Int) = RicherInt(i)
  
  implicit def toRicherLong(i:Long) = RicherLong(i)
  
  /**
   * Throws UnsupportedOperationException.
   * <br>
   * Wyrzuca UnsupportedOperationException.
   */
  def ??? = throw new UnsupportedOperationException("Not implemented yet")
  
  /**
   * Runs given block of code and prints to the standard output how long did it take.
   * <br>
   * Uruchamia dany blok kodu i wypisuje na standardowym wyjściu, ile to zajęło.
   */
  def timing(f: => Unit){
    val t0 = System.currentTimeMillis()
    f
    val t1 = System.currentTimeMillis()
    println("Time: "+(t1-t0)+" ms")
  }
  
  /**
   * Runs given block of code and prints to the standard output how long did it take.
   * Returns the result of the block. 
   * <br>
   * Uruchamia dany blok kodu i wypisuje na standardowym wyjściu, ile to zajęło.
   * Zwraca wynik tego bloku.
   */
  def timed[A](f: => A):A={
    val t0 = System.currentTimeMillis()
    val result:A = f
    val t1 = System.currentTimeMillis()
    println("Time: "+(t1-t0)+" ms")
    result
  }

}