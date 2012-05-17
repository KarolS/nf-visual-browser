/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query
import pl.umk.mat.stasiu88.nfserver._
import Protocols._
import scalaz._
import Scalaz._
import org.joda.time._
import org.joda.time.format.DateTimeFormat

class Statistic(val sumOver: List[Summable],
                val indexing: Indexing,
                val top: Int,
                var period: Period) {
  val backupPeriod = period

  override def toString() = {
    "Sum " + sumOver.mkString("[",",","]") + 
      ", get top " + top + "" +
      " grouping by (" + indexing + 
      "), for " + period
  }
}

sealed trait Indexing {
  def apply(q:Query, f: Flow): List[List[Int]]
  def decode(q:Query, index: List[Int]): (String, List[Int])
}

abstract class HostnameIndexing extends Indexing {
  def decode(q:Query, index: List[Int]) = index match {
    case 0 :: h :: xs => "from " + StringCache(h).name -> xs
    case 1 :: h :: xs => "to " + StringCache(h).name -> xs
  }
}
case object SrcHostnameIndex extends HostnameIndexing {
  def apply(q:Query, f: Flow) = List(
    List(0,StringCache(DnsCache.canonicalHostname(f.srcaddr)))
  )
}
case object DestHostnameIndex extends HostnameIndexing {
  def apply(q:Query, f: Flow) = List(
    List(1,StringCache(DnsCache.canonicalHostname(f.destaddr)))
  )
}
case object AnyHostnameIndex extends HostnameIndexing {
  def apply(q:Query, f: Flow) = List(
    List(0,StringCache(DnsCache.canonicalHostname(f.srcaddr))),
    List(1,StringCache(DnsCache.canonicalHostname(f.destaddr)))
  )
}
case class HostnameInDomainIndex(domain: Symbol) extends HostnameIndexing {
  def apply(q:Query, f: Flow) = 
    (DnsCache.inDomain(f.srcaddr, domain),
        DnsCache.inDomain(f.destaddr,domain)) match {
    case (false,false) =>
      Nil
    case (true,false) =>
      SrcHostnameIndex(q,f)
    case (true,true) =>
      AnyHostnameIndex(q,f)
    case (false,true) =>
      DestHostnameIndex(q,f)
  }
}
case class HostnameNotInDomainIndex(domain: Symbol) extends HostnameIndexing {
  def apply(q:Query, f: Flow) = 
    (! DnsCache.inDomain(f.srcaddr, domain),
        ! DnsCache.inDomain(f.destaddr,domain)) match {
    case (false,false) =>
      Nil
    case (true,false) =>
      SrcHostnameIndex(q,f)
    case (true,true) =>
      AnyHostnameIndex(q,f)
    case (false,true) =>
      DestHostnameIndex(q,f)
  }
}
abstract class IPIndexing extends Indexing {
  def decode(q:Query, index: List[Int]) = index match {
    case 0 :: 4 :: ip :: xs => "from " + IP4Addr(ip).toString -> xs
    case 1 :: 4 :: ip :: xs => "to " + IP4Addr(ip).toString -> xs
    //TODO
  }
}

case object SrcIPIndex extends IPIndexing {
  def apply(q:Query, f: Flow) = List(0 :: f.srcaddr.toIntList)
}
case object DestIPIndex extends IPIndexing {
  def apply(q:Query, f: Flow) = List(1 :: f.destaddr.toIntList)
}
case object AnyIPIndex extends IPIndexing {
  def apply(q:Query, f: Flow) = List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
}
case class IPInSubnetIndex(subnet: Subnet) extends IPIndexing {
  def apply(q:Query, f: Flow) = if (f.srcaddr ~ subnet) {
    if (f.destaddr ~ subnet) List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
    else List(0 :: f.srcaddr.toIntList)
  } else {
    if (f.destaddr ~ subnet) List(1 :: f.destaddr.toIntList)
    else List()
  }
}
case class IPNotInSubnetIndex(subnet: Subnet) extends IPIndexing {
  def apply(q:Query, f: Flow) = if (f.srcaddr ~ subnet) {
    if (f.destaddr ~ subnet) List()
    else List(1 :: f.destaddr.toIntList)
  } else {
    if (f.destaddr ~ subnet) List(0 :: f.srcaddr.toIntList)
    else List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
  }
}

sealed trait ListOfIndexing extends Indexing
case object NilIndex extends ListOfIndexing {
  def apply(q:Query, f: Flow) = List(List())
  def decode(q:Query, index: List[Int]) = "" -> index
  override def toString() = "()"
}
case class ConsIndex(head: Indexing, tail: ListOfIndexing) extends ListOfIndexing {
  def apply(q:Query, f: Flow) = head(q,f) |@| tail(q,f) apply { _ ++ _ }
  def decode(q:Query, index: List[Int]) = {
    val (headDecoded, rest1) = head.decode(q, index)
    val (tailDecoded, rest2) = tail.decode(q, rest1)
    (headDecoded + " " + tailDecoded, rest2)
  }
  override def toString() = head + ", " + tail
}

abstract class PortIndexing extends Indexing {
  def decode(q:Query, index: List[Int]) = index match {
    case 2 :: xs      => "no port" -> xs
    case 0 :: p :: xs => "from port " + p -> xs
    case 1 :: p :: xs => "to port " + p -> xs
  }
}
case object AnyPortIndex extends PortIndexing {
  def apply(q:Query, f: Flow) =
    if (f.protocol == TCP || f.protocol == UDP) List(List(0, f.srcport), List(1, f.destport))
    else List(List(2))
}
case class PortInRangeIndex(range:(Int,Int)) extends PortIndexing {
  def apply(q:Query, f: Flow):List[List[Int]] = {
    if (f.protocol == TCP || f.protocol == UDP){
      if(f.srcport>=range._1 && f.srcport<=range._2){
        if(f.destport>=range._1 && f.destport<=range._2)
          return List(List(0, f.srcport), List(1, f.destport))
        else
          return List(List(0, f.srcport))
      }
      else{
        if(f.destport>=range._1 && f.destport<=range._2)
          return List(List(1, f.destport))
        else
          return List()
      }
    }
    return List(List(2))
  }
}
case object SrcPortIndex extends PortIndexing {
  def apply(q:Query, f: Flow) =
    if (f.protocol == TCP || f.protocol == UDP) List(List(0, f.srcport))
    else List(List(2))
}
case object DestPortIndex extends PortIndexing {
  def apply(q:Query, f: Flow) =
    if (f.protocol == TCP || f.protocol == UDP) List(List(1, f.destport))
    else List(List(2))
}
case object ProtocolIndex extends Indexing {
  def apply(q:Query, f: Flow) = List(List(f.protocol))
  def decode(q:Query, index: List[Int]) = ("proto " + index.head, index.tail)
}
case object IPVersionIndex extends Indexing {
  def apply(q:Query, f: Flow) = List(List(f.srcaddr match {
    case _: IP4Addr => 4
  }))
  def decode(q:Query, index: List[Int]) = ("ipv" + index.head, index.tail)
}





sealed trait Summable {
  def apply(f: Flow): Long //TODO: a może BigInt?
}
case object Bytes extends Summable {
  def apply(f: Flow) = f.bytes
}
case object Packets extends Summable {
  def apply(f: Flow) = f.packets
}
case object Flows extends Summable {
  def apply(f: Flow) = 1
}
case object Duration extends Summable {
  def apply(f: Flow) = f.endTime - f.startTime
}

sealed trait Period {
  val DOW = Array("SUN","MON","TUE","WED","THU","FRI","SAT","SUN")
  val EPOCH = new Instant(0L)
  def apply(q:Query, f: Flow): Long
  def decode(index: Long): String
}

case class CheatingPeriod(constant: Long, decodeConstant: String) extends Period {
  def apply(q:Query, f: Flow) = constant
  def decode(index: Long) = decodeConstant
}

case object EachAlways extends Period {
  def apply(q:Query, f: Flow) = 0L
  def decode(index: Long) = "all time"
}

case object EachMinute extends Period {
  val FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  def apply(q:Query, f: Flow) = f.endTime / 60000L
  def decode(index: Long) = new DateTime(EPOCH.plus(index * (60*1000))).toString(FORMAT)
}

case object EachHour extends Period {
  val FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd HH")
  def apply(q:Query, f: Flow) = 
    Hours.hoursBetween(EPOCH, new Instant(f.endTime)).getHours().toLong
    
  def decode(index: Long) = new DateTime(EPOCH.plus(index * (3600*1000))).toString(FORMAT)
}

case object EachDay extends Period {
  val FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd")
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).dayOfMonth().roundFloorCopy().getMillis() / (24L*3600*1000)
    
  def decode(index: Long) = new DateTime(EPOCH.plus(index * (24L*3600*1000))).toString(FORMAT)
}

case object EachWeek extends Period {
  val FORMAT1 = DateTimeFormat.forPattern("yyyy-MM-dd")
  val FORMAT2 = DateTimeFormat.forPattern("MM-dd")
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).weekOfWeekyear().roundFloorCopy().getMillis() / (7*24L*3600*1000)
  def decode(index: Long) = {
    val d1 = new DateTime(EPOCH.plus(index * (7*24L*3600*1000))).toString(FORMAT1)
    val d2 = new DateTime(
      EPOCH.plus(index * (7*24L*3600*1000)).plus(org.joda.time.Duration.standardHours(6*24))
    ).toString(FORMAT2)
    d1+"/"+d2
  }
}

case object EachMonth extends Period {
  def apply(q:Query, f: Flow) = {
    val x = new DateTime(f.endTime, q.timeZone)
    x.getYear()*12 + x.getMonthOfYear()
  }
  
  def decode(index: Long) = {
    val corr = index-1
    val m = corr%12 +1
    val y = corr/12
    y+(if(m<10) "-0"+m else "-"+m)
  }
}

case object EachYear extends Period {
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).getYear()
    
  def decode(index: Long) = index.toString
}

case object EachDayOfWeek extends Period {
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).getDayOfWeek()
    
  def decode(index: Long) = DOW(index.toInt)
}

case object EachHourOfDay extends Period {
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).getHourOfDay()
    
  def decode(index: Long) = if(index<10) "0"+index else index.toString
}

case object EachHourOfWeek extends Period {
  def apply(q:Query, f: Flow) = { 
    val x = new DateTime(f.endTime, q.timeZone)
    x.getHourOfDay() + 24*x.getDayOfWeek()
  }
  
  def decode(index: Long) = DOW(index.toInt/24)+(if(index%24<10) "-0"+index%24 else "-"+index%24)
}


