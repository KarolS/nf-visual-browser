/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

final class Flow (
  val startTime:Long,
  val endTime: Long,
  val bytes: Long,
  val packets: Long,
  val srcaddr: Addr,
  val destaddr: Addr,
  val srcport: Int,
  val destport:Int
) {

}


abstract class Addr {
  def ~(mask:AddrMask):Boolean = mask(this)
}
case class IP4Addr(val value:Int) extends Addr{
  override def toString = {
	"%d.%d.%d.%d" format ((value>>>24)&255, (value>>>16)&255,
						  (value>>>8) &255, (value&255))
  }
}
object IP4Addr {
  def apply(string:String): IP4Addr = {
	val parts = string.split(".")
	if(parts.length!=4) throw new NumberFormatException
	var intparts = parts map {_.toInt}
	if (intparts exists {x=> x<0 || x>255} ) throw new NumberFormatException
	new IP4Addr((0/:intparts){_*256+_})
  }
}
abstract class AddrMask extends Function1[Addr,Boolean]

case class IP4AddrMask (subnet: Int, mask:Int) extends AddrMask{
  def apply(addr: Addr) = addr match {
	case IP4Addr(value) => (value&mask)==(subnet&mask)
	case _ => false
  }
}

object IP4AddrMask {
  def apply(subnet:String, maskBits:Integer){
	val ip = IP4Addr(subnet).value
	val mask = ~(1<<maskBits)
	new IP4AddrMask(ip, mask)
  }
}
