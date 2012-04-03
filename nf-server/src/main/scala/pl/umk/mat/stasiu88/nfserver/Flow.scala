/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

import org.joda.time.Instant

final class Flow(
  var srcaddr: Addr = IP4Addr.ZERO,
  var destaddr: Addr = IP4Addr.ZERO,
  var nextHop: Addr = IP4Addr.ZERO,
  var inputInterface: Int = 0,
  var outputInterface: Int = 0,
  var packets: Long = 0,
  var bytes: Long = 0,
  var startTime: Long = 0,
  var endTime: Long = 0,
  var srcport: Int = 0,
  var destport: Int = 0,
  var tcpFlags: Int = 0,
  var protocol: Int = 0,
  var tos: Byte = 0,
  var srcAS: Int = 0,
  var destAS: Int = 0
  ) {
  override def toString() = (
    Protocols.name(protocol) + " " +
    srcaddr + ":"+srcport + " -> " +
    destaddr + ":" + destport + " " +
    new Instant(startTime) + " - " +
    new Instant(endTime) + " " +
    bytes + "B " +
    packets + " packets"
  ) 
}

abstract class Addr {
  def ~(mask: Subnet): Boolean = mask(this)
  def toIntList: List[Int]
}
object Addr {
  val IPV4REGEX = """\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}"""r
  def apply(string: String): Addr = string match {
    case IPV4REGEX() => IP4Addr(string)
  }
}
case class IP4Addr(value: Int) extends Addr {
  override def toString = {
    "%d.%d.%d.%d" format ((value >>> 24) & 255, (value >>> 16) & 255,
      (value >>> 8) & 255, (value & 255))
  }
  def toIntList = List(4, value)
}
case class IP6Addr(part0: Long, part1: Long) extends Addr {
  def toIntList = List(6, (part0>>>32).toInt, (part0).toInt, (part1>>>32).toInt, (part1).toInt)
}
object IP4Addr {
  val ZERO = IP4Addr("0.0.0.0") 
  val LOCALHOST = IP4Addr("127.0.0.1") 
  def apply(string: String): IP4Addr = {
    val parts = string.split("""\.""")
    if (parts.length != 4) throw new NumberFormatException
    var intparts = parts map { _.toInt }
    if (intparts exists { x => x < 0 || x > 255 }) throw new NumberFormatException
    new IP4Addr((0 /: intparts){ _ * 256 + _ })
  }
}
object IP6Addr {
  val ZERO = new IP6Addr(0L, 0L) 
  val LOCALHOST = new IP6Addr(0L, 1L) 
}
abstract class Subnet extends Function1[Addr, Boolean] {
  def toIntList: List[Int]
}

object Subnet {
  def apply(subnet: Addr, mask: Addr): Subnet = (subnet, mask) match {
    case (IP4Addr(v), IP4Addr(w)) => IP4Subnet(v, w)
  }
  def apply(subnet: Addr, maskbits: Int): Subnet = subnet match {
    case IP4Addr(v) if maskbits >= 0 && maskbits <= 32 => IP4Subnet(v, ~((1 << (32 - maskbits)) - 1))
  }
  def apply(subnet: Addr): Subnet = subnet match {
    case IP4Addr(v) => IP4Subnet(v, -1)
  }
}
case class IP4Subnet(subnet: Int, mask: Int) extends Subnet {
  override def toString = IP4Addr(subnet) + "/" + IP4Addr(mask)
  def apply(addr: Addr) = addr match {
    case IP4Addr(value) => (value & mask) == (subnet & mask)
    case _              => false
  }
  def toIntList: List[Int] = List(4, subnet, mask)
}

object IP4Subnet {
  def apply(subnet: String, maskBits: Int) {
    val ip = IP4Addr(subnet).value
    val mask = ~((1 << (32 - maskBits)) - 1)
    new IP4Subnet(ip, mask)
  }
}

object SNMPInterface {
  
}
