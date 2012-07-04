/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.manager

import scala.util.Random

/**
 * Generates pseudo-random unique indentifiers.
 * <br>
 * Generuje pseudolosowe unikalne identyfikatory.
 */
object SymbolFactory {
  private [this] var counter: Int = Random.nextInt(1<<12) 
  private [this] var salt: Int = Random.nextInt(1<<12) 
  def next():Symbol = {
    counter += 1
    Symbol( Integer.toString(counter,36)+"_"+Integer.toString((counter+salt).toString()##,36) )
  }
}