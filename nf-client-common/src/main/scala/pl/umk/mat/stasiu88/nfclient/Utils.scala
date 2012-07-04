/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient

import com.sun.corba.se.impl.orbutil.threadpool.ThreadPoolManagerImpl

object Utils {

  /**
   * Executes <code>wrapper</code> if the partial function is defined for given argument.
   * <br>
   * Wykonuje <code>wrapper</code>, jeśli funkcja częściowa jest zdefiniowana dla danego argumentu.
   */
  def forAnyValidCase[Z](wrapper: Z=>Unit)(pf: PartialFunction[Z,Unit]):PartialFunction[Z,Unit] = {
    case i if pf isDefinedAt i =>
      try{
        wrapper(i)
        pf(i)
      } catch { case th=>
        println(th)
        throw th
      }
  }
  
  /**
   * Creates a thread from a block of code
   * <br>
   * Tworzy wątek z bloku kodu.
   */
  def thread(function: =>Unit) = new Thread(new Runnable{def run = function})

  /**
   * Allows for converting an option to a list and prepending an option to a list.
   * <br>
   * Pozwala na konwersję opcji na listę i dodawanie opcji na początek listy.
   */
  implicit def optionToList[A](o: Option[A]) = new {
    def toList = o match{
      case None => List()
      case Some(a) => List(a)
    }
    def +[B>:A](xs: List[B]) = o match{
      case None => xs
      case Some(x) => x::xs
    }
  }
}