/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver._

sealed trait Filter {
  def matches(flow: Flow): Boolean
  def replaceSubnets(subnets: Map[String, Subnet]):Filter
}
case class OrFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " or ", ")")
  def matches(flow: Flow) = filters exists { _ matches flow }
  def replaceSubnets(subnets: Map[String, Subnet]) = {
    OrFilter(filters map (_.replaceSubnets(subnets)))
  }
}
case class AndFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " and ", ")")
  def matches(flow: Flow) = filters forall { _ matches flow }
  def replaceSubnets(subnets: Map[String, Subnet]) = {
    AndFilter(filters map (_.replaceSubnets(subnets)))
  }
}
case class NotFilter(filter: Filter) extends Filter {
  override def toString = "not (" + filter + ")"
  def matches(flow: Flow) = !filter.matches(flow)
  def replaceSubnets(subnets: Map[String, Subnet]) = NotFilter(filter replaceSubnets subnets)
}

case object AllFilter extends Filter {
  override def toString = "all"
  def matches(flow: Flow) = true
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}

case object IP4Filter extends Filter {
  override def toString = "IPv4"
  def matches(flow: Flow) = flow.srcaddr.isInstanceOf[IP4Addr]
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}
case object IP6Filter extends Filter {
  override def toString = "IPv6"
  def matches(flow: Flow) = !flow.srcaddr.isInstanceOf[IP4Addr] //TODO
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}

case class IPMaskFilter(designation: Designation[Addr], mask: Subnet) extends Filter {
  override def toString = designation + " IP in " + mask
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ ~ mask)
  def replaceSubnets(subnets: Map[String, Subnet]) = mask match {
    case NamedSubnet(n) => IPMaskFilter(designation, subnets(n))
    case _ => this
  }
}
case class IPEqualFilter(designation: Designation[Addr], ip: Addr) extends Filter {
  override def toString = designation + " IP = " + ip
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ == ip)
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}
case class ProtocolFilter(protocol: Int) extends Filter {
  override def toString = Protocols.name(protocol)
  def matches(flow: Flow) = flow.protocol == protocol
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}

case class PortEqualFilter(designation: Designation[Int], port: Int) extends Filter {
  override def toString = designation + " port = " + port
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, _ == port)
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}
case class PortRelationalFilter(designation: Designation[Int],
                                relationaloperator: (Int => (Int => Boolean)),
                                port: Int) extends Filter {
  private val function = relationaloperator(port)
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, function)
  def replaceSubnets(subnets: Map[String, Subnet]) = this
  override def toString = designation + " port "+ relationaloperator + " " + port
}
case class PortRangeFilter(designation: Designation[Int],
                           from: Int, to: Int) extends Filter {
  override def toString = designation + " port in " + from + ".." + to
  private val function = (x: Int) => x >= from && x <= to
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, function)
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}


case class HostEqualFilter(designation: Designation[Addr], hostname: Symbol) extends Filter {
  override def toString = designation + " host = " + hostname.name
  private val function = (x: Addr) => DnsCache.equal(x, hostname)
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, function)
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}
case class HostInDomainFilter(designation: Designation[Addr], domain: Symbol) extends Filter {
  override def toString = designation + " host in " + domain.name
  private val function = (x: Addr) => DnsCache.inDomain(x, domain)
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, function)
  def replaceSubnets(subnets: Map[String, Subnet]) = this
}
//TODO: inne rodzaje filtrów

