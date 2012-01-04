/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver.Addr

trait Designation[A] {
  def matches(src: A, dst: A, predicate: (A => Boolean)): Boolean
}

case object SrcIpDesignation extends Designation[Addr] {
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(src)
}
case object DstIpDesignation extends Designation[Addr] {
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(dst)
}
case object AnyIpDesignation extends Designation[Addr] {
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(src) || predicate(dst)
}
case object BothIpDesignation extends Designation[Addr] {
  def matches(src: Addr, dst: Addr, predicate: (Addr => Boolean)) = predicate(src) && predicate(dst)
}

case object SrcIntDesignation extends Designation[Int] {
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(src)
}
case object DstIntDesignation extends Designation[Int] {
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(dst)
}
case object AnyIntDesignation extends Designation[Int] {
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(src) || predicate(dst)
}
case object BothIntDesignation extends Designation[Int] {
  def matches(src: Int, dst: Int, predicate: (Int => Boolean)) = predicate(src) && predicate(dst)
}

