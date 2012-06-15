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
case class PortNotInRangeIndex(range:(Int,Int)) extends PortIndexing {
  def apply(q:Query, f: Flow):List[List[Int]] = {
    if (f.protocol == TCP || f.protocol == UDP){
      if(f.srcport>=range._1 && f.srcport<=range._2){
        if(f.destport>=range._1 && f.destport<=range._2)
          return List()
        else
          return List(List(1, f.srcport))
      }
      else{
        if(f.destport>=range._1 && f.destport<=range._2)
          return List(List(0, f.destport))
        else
          return List(List(0, f.srcport), List(1, f.destport))
      }
    }
    return List(List(2))
  }
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
  def decode(q:Query, index: List[Int]) = (Protocol.name(index.head), index.tail)
}
case object IPVersionIndex extends Indexing {
  def apply(q:Query, f: Flow) = List(List(f.srcaddr match {
    case _: IP4Addr => 4
    case _: IP6Addr => 4
    case _ => 0
  }))
  def decode(q:Query, index: List[Int]) = ("IPv" + index.head, index.tail)
}

case class TosIndex(mask: Int) extends Indexing {
  def apply(q:Query, f: Flow) = List(List(f.tos & mask))
  
  def decode(q:Query, index: List[Int]) = ("TOS & " + mask " = " + index.head, index.tail)
}
