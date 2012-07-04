/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver.Addr

/**
 * Applies a predicate to some of the parameters.
 * <br>
 * Aplikuje predykat do niektórych parametrów.
 */
trait Designation[@specialized(Int) A] {
  def matches(src: A, dst: A, predicate: (A => Boolean)): Boolean
}

/**
 * Applies predicate to the first argument.
 * <br>
 * Aplikuje predykat do pierwszego argumentu.
 */
case object SrcIpDesignation extends Designation[Addr] {
  override def toString = "source"
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(src)
}
/**
 * Applies predicate to the second argument.
 * <br>
 * Aplikuje predykat do drugiego argumentu.
 */
case object DstIpDesignation extends Designation[Addr] {
  override def toString = "destination"
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(dst)
}
/**
 * Applies predicate to both arguments and returns true if at least one of them yields true  
 * <br>
 * Aplikuje predykat do obu argumentów i zwraca true jeśli co najmniej jeden z nich da true
 */
case object AnyIpDesignation extends Designation[Addr] {
  override def toString = "any"
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(src) || predicate(dst)
}
/**
 * Applies predicate to both arguments and returns true if both of them yield true  
 * <br>
 * Aplikuje predykat do obu argumentów i zwraca true jeśli oba dadzą true
 */
case object BothIpDesignation extends Designation[Addr] {
  override def toString = "both"
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(src) && predicate(dst)
}

/**
 * Applies predicate to the first argument.
 * <br>
 * Aplikuje predykat do pierwszego argumentu.
 */
case object SrcIntDesignation extends Designation[Int] {
  override def toString = "source"
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(src)
}
/**
 * Applies predicate to the second argument.
 * <br>
 * Aplikuje predykat do drugiego argumentu.
 */
case object DstIntDesignation extends Designation[Int] {
  override def toString = "destination"
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(dst)
}
/**
 * Applies predicate to both arguments and returns true if at least one of them yields true  
 * <br>
 * Aplikuje predykat do obu argumentów i zwraca true jeśli co najmniej jeden z nich da true
 */
case object AnyIntDesignation extends Designation[Int] {
  override def toString = "any"
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(src) || predicate(dst)
}
/**
 * Applies predicate to both arguments and returns true if both of them yield true  
 * <br>
 * Aplikuje predykat do obu argumentów i zwraca true jeśli oba dadzą true
 */
case object BothIntDesignation extends Designation[Int] {
  override def toString = "both"
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(src) && predicate(dst)
}

