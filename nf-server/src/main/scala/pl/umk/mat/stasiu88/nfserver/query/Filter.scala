/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver._

sealed trait Filter {
  def matches(flow: Flow): Boolean
}
case class OrFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " | ", ")")
  def matches(flow: Flow) = filters exists { _ matches flow }
}
case class AndFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " & ", ")")
  def matches(flow: Flow) = filters forall { _ matches flow }
}
case class NotFilter(filter: Filter) extends Filter {
  override def toString = "!(" + filter + ")"
  def matches(flow: Flow) = !filter.matches(flow)
}

case object AllFilter extends Filter {
  override def toString = "all"
  def matches(flow: Flow) = true
}

case object IP4Filter extends Filter {
  def matches(flow: Flow) = flow.srcaddr.isInstanceOf[IP4Addr]
}
case object IP6Filter extends Filter {
  def matches(flow: Flow) = !flow.srcaddr.isInstanceOf[IP4Addr] //TODO
}

case class IPMaskFilter(designation: Designation[Addr], mask: Subnet) extends Filter {
  override def toString = designation + " in " + mask
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ ~ mask)
}
case class IPEqualFilter(designation: Designation[Addr], ip: Addr) extends Filter {
  override def toString = designation + " = " + ip
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ == ip)
}
case class ProtocolFilter(protocol: Int) extends Filter {
  override def toString = "proto " + protocol
  def matches(flow: Flow) = flow.protocol == protocol
}

case class PortEqualFilter(designation: Designation[Int], port: Int) extends Filter {
  override def toString = designation + " = " + port
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, _ == port)
}
case class PortRelationalFilter(designation: Designation[Int],
                                relationaloperator: (Int => (Int => Boolean)),
                                port: Int) extends Filter {
  private val function = relationaloperator(port)
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, function)
}
case class PortRangeFilter(designation: Designation[Int],
                           from: Int, to: Int) extends Filter {
  override def toString = designation + " in " + from + ".." + to
  private val function = (x: Int) => x >= from && x <= to
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, function)
}

//TODO: inne rodzaje filtrów

