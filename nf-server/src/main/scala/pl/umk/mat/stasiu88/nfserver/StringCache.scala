package pl.umk.mat.stasiu88.nfserver

import scala.collection.mutable

object StringCache {
  private[this] val indexer =  mutable.Map[Symbol,Int]()
  private[this] val cache = new mutable.ArrayBuffer[Symbol]
  def apply(i:Int): Symbol = {
    if(i<0 || i>=cache.length) Symbol(i.toString)
    else cache(i)
  }
  def apply(s: Symbol): Int = {
    indexer.getOrElse(s, synchronized {
      val i = cache.length
      cache += s
      indexer(s) = i
      i
    })
  }
  def apply(s: String): Int = apply(Symbol(s))
}