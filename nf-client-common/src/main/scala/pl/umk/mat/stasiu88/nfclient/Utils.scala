package pl.umk.mat.stasiu88.nfclient

import com.sun.corba.se.impl.orbutil.threadpool.ThreadPoolManagerImpl

object Utils {

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
  
  def thread(function: =>Unit) = new Thread(new Runnable{def run = function})

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