/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

import scala.collection.mutable

/**
 * A string cache, mapping strings and symbols to integers and vice versa.
 * <br>
 * Cache łańcuchów, mapujący łańcuchy i symbole na liczby całkowite i odwrotnie.
 */
object StringCache {
  private[this] val indexer =  mutable.Map[Symbol,Int]()
  private[this] val cache = new mutable.ArrayBuffer[Symbol]
  /**
   * Returns a symbol for given number.
   * <br>
   * Zwraca symbol dla danej liczby.
   */
  def apply(i:Int): Symbol = {
    if(i<0 || i>=cache.length) Symbol(i.toString)
    else cache(i)
  }
  /**
   * Returns a number for given symbol, adding it to cache if necessary.
   * <br>
   * Zwraca liczbę dla danego symbolu, dodając go do cache, jeśli potrzeba.
   */
  def apply(s: Symbol): Int = {
    indexer.getOrElse(s, synchronized {
      val i = cache.length
      cache += s
      indexer(s) = i
      i
    })
  }
  /**
   * Returns a number for given string, adding it to cache if necessary.
   * <br>
   * Zwraca liczbę dla danego łańcucha, dodając go do cache, jeśli potrzeba.
   */
  def apply(s: String): Int = apply(Symbol(s))
}