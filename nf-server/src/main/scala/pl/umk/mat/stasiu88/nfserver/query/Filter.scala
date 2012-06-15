/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver._

sealed trait Filter {
  def matches(flow: Flow): Boolean
  def replaceSubnets(subnets: Map[String, Subnet]):Filter = this
}
case class OrFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " or ", ")")
  def matches(flow: Flow) = filters exists { _ matches flow }
  override def replaceSubnets(subnets: Map[String, Subnet]) = {
    OrFilter(filters map (_.replaceSubnets(subnets)))
  }
}
case class AndFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " and ", ")")
  def matches(flow: Flow) = filters forall { _ matches flow }
  override def replaceSubnets(subnets: Map[String, Subnet]) = {
    AndFilter(filters map (_.replaceSubnets(subnets)))
  }
}
case class NotFilter(filter: Filter) extends Filter {
  override def toString = "not (" + filter + ")"
  def matches(flow: Flow) = !filter.matches(flow)
  override def replaceSubnets(subnets: Map[String, Subnet]) = NotFilter(filter replaceSubnets subnets)
}

case object AllFilter extends Filter {
  override def toString = "all"
  def matches(flow: Flow) = true
}

case object IP4Filter extends Filter {
  override def toString = "IPv4"
  def matches(flow: Flow) = flow.srcaddr.isInstanceOf[IP4Addr]
}
case object IP6Filter extends Filter {
  override def toString = "IPv6"
  def matches(flow: Flow) = !flow.srcaddr.isInstanceOf[IP4Addr] //TODO
}

case class IPMaskFilter(designation: Designation[Addr], mask: Subnet) extends Filter {
  override def toString = designation + " IP in " + mask
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ ~ mask)
  override def replaceSubnets(subnets: Map[String, Subnet]) = mask match {
    case NamedSubnet(n) => IPMaskFilter(designation, subnets(n))
    case _ => this
  }
}
case class IPEqualFilter(designation: Designation[Addr], ip: Addr) extends Filter {
  override def toString = designation + " IP = " + ip
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ == ip)
}
case class ProtocolFilter(protocol: Int) extends Filter {
  override def toString = Protocols.name(protocol)
  def matches(flow: Flow) = flow.protocol == protocol
}

case class PortEqualFilter(designation: Designation[Int], port: Int) extends Filter {
  override def toString = designation + " port = " + port
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, _ == port)
}
case class PortRelationalFilter(designation: Designation[Int],
                                relationaloperator: (Int => (Int => Boolean)),
                                port: Int) extends Filter {
  private val function = relationaloperator(port)
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, function)
  override def toString = designation + " port "+ relationaloperator + " " + port
}
case class PortRangeFilter(designation: Designation[Int],
                           from: Int, to: Int) extends Filter {
  override def toString = designation + " port in " + from + ".." + to
  private val function = (x: Int) => x >= from && x <= to
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, function)
}


case class HostEqualFilter(designation: Designation[Addr], hostname: Symbol) extends Filter {
  override def toString = designation + " host = " + hostname.name
  private val function = (x: Addr) => DnsCache.equal(x, hostname)
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, function)
}
case class HostInDomainFilter(designation: Designation[Addr], domain: Symbol) extends Filter {
  override def toString = designation + " host in " + domain.name
  private val function = (x: Addr) => DnsCache.inDomain(x, domain)
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, function)
}

case class InterfaceEqualFilter(designation: Designation[Int], interfaceId: Int) extends Filter {
  override def toString = designation + " interface " + interfaceId
  def matches(flow: Flow) = designation.matches(flow.inputInterface, flow.outputInterface, _ == interfaceId)
}

case class TosEqualFilter(mask:Int, tos: Int) extends Filter {
  override def toString = "ToS & " + mask + " = " + tos
  def matches(flow: Flow) = (flow.tos & mask) == tos
}
case class TosAllFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " = " + mask
  def matches(flow: Flow) = (flow.tos & mask) == mask
}
case class TosNoneFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " = 0"
  def matches(flow: Flow) = (flow.tos & mask) == 0
}
case class TosNotAllFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " != " + mask
  def matches(flow: Flow) = (flow.tos & mask) != mask
}
case class TosAnyFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " != 0"
  def matches(flow: Flow) = (flow.tos & mask) != 0
}

//TODO: inne rodzaje filtrów

