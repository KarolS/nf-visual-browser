/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver._

/**
 * A predicate working on flow records.
 * <br>
 * Predykat nad rekordami przepływów.
 */
sealed trait Filter {
  /**
   * Returns true if the flow satisfied the filter.
   * <br>
   * Zwraca true, jeśli przepływ spełnia warunki filtra.
   */
  def matches(flow: Flow): Boolean
  /**
   * Returns a filter with subnet names replaced by actual subnets.
   * <br>
   * Zwraca filtr z nazwami podsieci zastąpionymi właściwymi podsieciami.
   */
  def replaceSubnets(subnets: Map[String, Subnet]):Filter = this
}
/**
 * Matches all flows that match at least one of the subfilters.
 * <br>
 * Przepuszcza przepływy, które przepuszane są przez co najmniej jeden z podfiltrów.
 */
case class OrFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " or ", ")")
  def matches(flow: Flow) = filters exists { _ matches flow }
  override def replaceSubnets(subnets: Map[String, Subnet]) = {
    OrFilter(filters map (_.replaceSubnets(subnets)))
  }
}
/**
 * Matches all flows that match all the subfilters.
 * <br>
 * Przepuszcza przepływy, które przepuszane są przez wszystkie podfiltry.
 */
case class AndFilter(filters: List[Filter]) extends Filter {
  override def toString = filters.mkString("(", " and ", ")")
  def matches(flow: Flow) = filters forall { _ matches flow }
  override def replaceSubnets(subnets: Map[String, Subnet]) = {
    AndFilter(filters map (_.replaceSubnets(subnets)))
  }
}
/**
 * Matches all flows not matched by the subfilter.
 * <br>
 * Przepuszcza przepływy, których nie przepusza podfiltr.
 */
case class NotFilter(filter: Filter) extends Filter {
  override def toString = "not (" + filter + ")"
  def matches(flow: Flow) = !filter.matches(flow)
  override def replaceSubnets(subnets: Map[String, Subnet]) = NotFilter(filter replaceSubnets subnets)
}

/**
 * Matches all flows.
 * <br>
 * Przepuszcza wszystkie przepływy.
 */
case object AllFilter extends Filter {
  override def toString = "all"
  def matches(flow: Flow) = true
}
/**
 * Matches all IPv4 flows.
 * <br>
 * Przepuszcza przepływy IPv4
 */
case object IP4Filter extends Filter {
  override def toString = "IPv4"
  def matches(flow: Flow) = flow.srcaddr.isInstanceOf[IP4Addr]
}
/**
 * Matches all IPv6 flows.
 * <br>
 * Przepuszcza przepływy IPv6
 */
case object IP6Filter extends Filter {
  override def toString = "IPv6"
  def matches(flow: Flow) = !flow.srcaddr.isInstanceOf[IP4Addr] //TODO
}
/**
 * Matches all flows where designated address belongs to the subnet.
 * <br>
 * Przepuszcza przepływy, w których wybrany adres należy do danej podsieci.
 */
case class IPMaskFilter(designation: Designation[Addr], mask: Subnet) extends Filter {
  override def toString = designation + " IP in " + mask
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ ~ mask)
  override def replaceSubnets(subnets: Map[String, Subnet]) = mask match {
    case NamedSubnet(n) => IPMaskFilter(designation, subnets(n))
    case _ => this
  }
}
/**
 * Matches all flows where designated address is equal to given address.
 * <br>
 * Przepuszcza przepływy, w których wybrany adres równa się danemu.
 */
case class IPEqualFilter(designation: Designation[Addr], ip: Addr) extends Filter {
  override def toString = designation + " IP = " + ip
  def matches(flow: Flow) = designation.matches(flow.srcaddr, flow.destaddr, _ == ip)
}
/**
 * Matches all flows using given protocol.
 * <br>
 * Przepuszcza przepływy danego protokołu.
 */
case class ProtocolFilter(protocol: Int) extends Filter {
  override def toString = Protocols.name(protocol)
  def matches(flow: Flow) = flow.protocol == protocol
}

/**
 * Matches all flows where designated port is equal to given port.
 * <br>
 * Przepuszcza przepływy, w których wybrany port równa się danemu.
 */
case class PortEqualFilter(designation: Designation[Int], port: Int) extends Filter {
  override def toString = designation + " port = " + port
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, _ == port)
}
/**
 * Matches all flows where designated port satisfies given relation with given port.
 * <br>
 * Przepuszcza przepływy, w których wybrany port spełnia danę relację z danym portem.
 */
case class PortRelationalFilter(designation: Designation[Int],
                                relationaloperator: (Int => (Int => Boolean)),
                                port: Int) extends Filter {
  private val function = relationaloperator(port)
  def matches(flow: Flow) = designation.matches(flow.srcport, flow.destport, function)
  override def toString = designation + " port "+ relationaloperator + " " + port
}
/**
 * Matches all flows where designated port belongs to a given range.
 * <br>
 * Przepuszcza przepływy, w których wybrany port należy do danego zakresu.
 */
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

/**
 * Matches all flows where designated interface is equal to given interface.
 * <br>
 * Przepuszcza przepływy, w których wybrany interfejs równa się danemu.
 */
case class InterfaceEqualFilter(designation: Designation[Int], interfaceId: Int) extends Filter {
  override def toString = designation + " interface " + interfaceId
  def matches(flow: Flow) = designation.matches(flow.inputInterface, flow.outputInterface, _ == interfaceId)
}

/**
 * Matches all flows where ToS byte is equal to given byte.
 * <br>
 * Przepuszcza przepływy, w których bajt ToS równa się danemu.
 */
case class TosEqualFilter(mask:Int, tos: Int) extends Filter {
  override def toString = "ToS & " + mask + " = " + tos
  def matches(flow: Flow) = (flow.tos & mask) == tos
}
/**
 * Matches all flows where ToS byte has all given bits set.
 * <br>
 * Przepuszcza przepływy, w których bajt ToS ma włączone wszystkie dane bity.
 */
case class TosAllFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " = " + mask
  def matches(flow: Flow) = (flow.tos & mask) == mask
}
/**
 * Matches all flows where ToS byte has all given bits clear.
 * <br>
 * Przepuszcza przepływy, w których bajt ToS ma wyłączone wszystkie dane bity.
 */
case class TosNoneFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " = 0"
  def matches(flow: Flow) = (flow.tos & mask) == 0
}
/**
 * Matches all flows where ToS byte has not all give\n bits set.
 * <br>
 * Przepuszcza przepływy, w których bajt ToS ma włączone niewszystkie dane bity.
 */
case class TosNotAllFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " != " + mask
  def matches(flow: Flow) = (flow.tos & mask) != mask
}
/**
 * Matches all flows where ToS byte has not all given bits clear.
 * <br>
 * Przepuszcza przepływy, w których bajt ToS ma wyłączone nie wszystkie dane bity.
 */
case class TosAnyFilter(mask: Int) extends Filter {
  override def toString = "ToS & " + mask + " != 0"
  def matches(flow: Flow) = (flow.tos & mask) != 0
}

//TODO: inne rodzaje filtrów

