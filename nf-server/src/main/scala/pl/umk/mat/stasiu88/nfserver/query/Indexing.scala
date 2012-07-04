/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query
import pl.umk.mat.stasiu88.nfserver._
import Protocols._
import scalaz._
import Scalaz._
import org.joda.time._
import org.joda.time.format.DateTimeFormat

/**
 * Assigns indexes to a flow.
 * <br>
 * Przypisuje indeksy przepływowi.
 */
sealed trait Indexing {
  /**
   * Assigns a list of indexes to a flow.
   * <br>
   * Przypisuje przepływowi listę indeksów.
   */
  def apply(q:Query, f: Flow): List[List[Int]]
  /**
   * Converts a prefix of the index to a string and returns it with the remaining part of the index.
   * <br>
   * Konwertuje prefiks indeksu na łańcuch i zwraca go z pozostałą częścią indeksu.
   */
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
/**
 * Base class for IP-based indexing.
 * <br>
 * Klasa bazowa dla indeksacji opartych o IP.
 */
abstract class IPIndexing extends Indexing {
  def decode(q:Query, index: List[Int]) = index match {
    case 0 :: 4 :: ip :: xs => "from " + IP4Addr(ip).toString -> xs
    case 1 :: 4 :: ip :: xs => "to " + IP4Addr(ip).toString -> xs
    case 0 :: 6 :: ip0 :: ip1 :: ip2 :: ip3 :: xs => "from " + IP6Addr(ip0,ip1,ip2,ip3).toString -> xs
    case 1 :: 6 :: ip0 :: ip1 :: ip2 :: ip3 :: xs => "to " + IP6Addr(ip0,ip1,ip2,ip3).toString -> xs
    //TODO
  }
}
/**
 * Indexes by source IP address.
 * <br>
 * Indeksuje w oparciu o źródłowy adres IP.
 */
case object SrcIPIndex extends IPIndexing {
  def apply(q:Query, f: Flow) = List(0 :: f.srcaddr.toIntList)
}
/**
 * Indexes by destination IP address.
 * <br>
 * Indeksuje w oparciu o docelowy adres IP.
 */
case object DestIPIndex extends IPIndexing {
  def apply(q:Query, f: Flow) = List(1 :: f.destaddr.toIntList)
}
/**
 * Indexes by both IP addresses.
 * <br>
 * Indeksuje w oparciu o oba adresy IP.
 */
case object AnyIPIndex extends IPIndexing {
  def apply(q:Query, f: Flow) = List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
}
/**
 * Indexes by IP addresses in a subnet.
 * <br>
 * Indeksuje w oparciu o adresy IP w podsieci.
 */
case class IPInSubnetIndex(subnet: Subnet) extends IPIndexing {
  def apply(q:Query, f: Flow) = if (f.srcaddr ~ subnet) {
    if (f.destaddr ~ subnet) List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
    else List(0 :: f.srcaddr.toIntList)
  } else {
    if (f.destaddr ~ subnet) List(1 :: f.destaddr.toIntList)
    else List()
  }
}
/**
 * Indexes by IP addresses outside of a subnet.
 * <br>
 * Indeksuje w oparciu o adresy IP poza podsiecią.
 */
case class IPNotInSubnetIndex(subnet: Subnet) extends IPIndexing {
  def apply(q:Query, f: Flow) = if (f.srcaddr ~ subnet) {
    if (f.destaddr ~ subnet) List()
    else List(1 :: f.destaddr.toIntList)
  } else {
    if (f.destaddr ~ subnet) List(0 :: f.srcaddr.toIntList)
    else List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
  }
}

/**
 * Assigns to a flow indexes that are concatenations of several indexes.
 * <br>
 * Przypisuje przepływowi indeksy, będące konkatenacją kilku indeksów. 
 */
sealed trait ListOfIndexing extends Indexing
/**
 * Assigns an empty index.
 * <br>
 * Przypisuje pusty indeks.
 */
case object NilIndex extends ListOfIndexing {
  def apply(q:Query, f: Flow) = List(List())
  def decode(q:Query, index: List[Int]) = "" -> index
  override def toString() = "()"
}
/**
 * Assigns to a flow a list of indexes, 
 * each of them being a concatenation of some index assigned by head and some index assigned by tail.
 * <br>
 * Przypisuje przepływowi listę indeksów,
 * każdy z nich będący konkatenacją pewnego indeksu przypisanego przez czoło i pewnego przez ogon.
 */
case class ConsIndex(head: Indexing, tail: ListOfIndexing) extends ListOfIndexing {
  def apply(q:Query, f: Flow) = head(q,f) |@| tail(q,f) apply { _ ++ _ }
  def decode(q:Query, index: List[Int]) = {
    val (headDecoded, rest1) = head.decode(q, index)
    val (tailDecoded, rest2) = tail.decode(q, rest1)
    (headDecoded + " " + tailDecoded, rest2)
  }
  override def toString() = head + ", " + tail
}

/**
 * Base class for port-based indexing.
 * <br>
 * Klasa bazowa dla indeksacji opartych o port.
 */
abstract class PortIndexing extends Indexing {
  def decode(q:Query, index: List[Int]) = index match {
    case 2 :: xs      => "no port" -> xs
    case 0 :: p :: xs => "from port " + p -> xs
    case 1 :: p :: xs => "to port " + p -> xs
  }
}
/**
 * Indexes by both ports.
 * <br>
 * Indeksuje w oparciu o oba porty.
 */
case object AnyPortIndex extends PortIndexing {
  def apply(q:Query, f: Flow) =
    if (f.protocol == TCP || f.protocol == UDP) List(List(0, f.srcport), List(1, f.destport))
    else List(List(2))
}
/**
 * Indexes by ports outside the range.
 * <br>
 * Indeksuje w oparciu o porty poza zakresem.
 */
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
/**
 * Indexes by ports in a range.
 * <br>
 * Indeksuje w oparciu o porty z zakresu.
 */
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
/**
 * Indexes by source port.
 * <br>
 * Indeksuje w oparciu o port źródłowy.
 */
case object SrcPortIndex extends PortIndexing {
  def apply(q:Query, f: Flow) =
    if (f.protocol == TCP || f.protocol == UDP) List(List(0, f.srcport))
    else List(List(2))
}
/**
 * Indexes by destination port.
 * <br>
 * Indeksuje w oparciu o port docelowy.
 */
case object DestPortIndex extends PortIndexing {
  def apply(q:Query, f: Flow) =
    if (f.protocol == TCP || f.protocol == UDP) List(List(1, f.destport))
    else List(List(2))
}
/**
 * Indexes by protocol.
 * <br>
 * Indeksuje w oparciu o protokół.
 */
case object ProtocolIndex extends Indexing {
  def apply(q:Query, f: Flow) = List(List(f.protocol))
  def decode(q:Query, index: List[Int]) = (Protocols.name(index.head), index.tail)
}
/**
 * Indexes by IP version.
 * <br>
 * Indeksuje w oparciu o wersję IP.
 */
case object IPVersionIndex extends Indexing {
  def apply(q:Query, f: Flow) = List(List(f.srcaddr match {
    case _: IP4Addr => 4
    case _: IP6Addr => 6
    case _ => 0
  }))
  def decode(q:Query, index: List[Int]) = ("IPv" + index.head, index.tail)
}

/**
 * Indexes by bits in ToS byte.
 * <br>
 * Indeksuje w oparciu o bity w bajcie ToS.
 */
case class TosIndex(mask: Int) extends Indexing {
  def apply(q:Query, f: Flow) = List(List(f.tos & mask))
  
  def decode(q:Query, index: List[Int]) = ("TOS & " + mask + " = " + index.head, index.tail)
}
