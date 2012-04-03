package pl.umk.mat.stasiu88.nfserver.manager

import scala.util.Random

object SymbolFactory {
  private [this] var counter: Int = Random.nextInt(1<<12) 
  def next():Symbol = {
    counter += 1
    Symbol( Integer.toString(counter,36)+"_"+Integer.toString(counter.toString()##,36) )
  }
}