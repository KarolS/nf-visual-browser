/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query
import pl.umk.mat.stasiu88.nfserver._
import Protocols._
import scalaz._
import Scalaz._

class Statistic(val sumOver: List[Summable],
                val indexing: Indexing,
                val top: Int,
                var period: Period) {
  val backupPeriod = period

}

sealed trait Indexing {
  def apply(f: Flow): List[List[Int]]
  def decode(index: List[Int]): (String, List[Int])
}

abstract class IPIndexing extends Indexing {
  def decode(index: List[Int]) = index match {
    case 0 :: 4 :: ip :: xs => "from " + IP4Addr(ip).toString -> xs
    case 1 :: 4 :: ip :: xs => "to " + IP4Addr(ip).toString -> xs
  }
}

object SrcIPIndex extends IPIndexing {
  def apply(f: Flow) = List(0 :: f.srcaddr.toIntList)

}
object DestIPIndex extends IPIndexing {
  def apply(f: Flow) = List(1 :: f.destaddr.toIntList)
}
object AnyIPIndex extends IPIndexing {
  def apply(f: Flow) = List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
}
class IPInSubnetIndex(subnet: Subnet) extends IPIndexing {
  def apply(f: Flow) = if (f.srcaddr ~ subnet) {
    if (f.destaddr ~ subnet) List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
    else List(0 :: f.srcaddr.toIntList)
  } else {
    if (f.destaddr ~ subnet) List(1 :: f.destaddr.toIntList)
    else List()
  }
}
class IPNotInSubnetIndex(subnet: Subnet) extends IPIndexing {
  def apply(f: Flow) = if (f.srcaddr ~ subnet) {
    if (f.destaddr ~ subnet) List()
    else List(1 :: f.destaddr.toIntList)
  } else {
    if (f.destaddr ~ subnet) List(0 :: f.srcaddr.toIntList)
    else List(0 :: f.srcaddr.toIntList, 1 :: f.destaddr.toIntList)
  }
}

sealed trait ListOfIndexing extends Indexing
object NilIndex extends ListOfIndexing {
  def apply(f: Flow) = List(List())
  def decode(index: List[Int]) = "" -> index
}
class ConsIndex(head: Indexing, tail: ListOfIndexing) extends ListOfIndexing {
  def apply(f: Flow) = head(f) |@| tail(f) apply { _ ++ _ }
  def decode(index: List[Int]) = {
    val (headDecoded, rest1) = head.decode(index)
    val (tailDecoded, rest2) = tail.decode(rest1)
    (headDecoded + " " + tailDecoded, rest2)
  }
}

object PortIndex extends Indexing {
  def apply(f: Flow) =
    if (f.protocol == TCP || f.protocol == UDP) List(List(0, f.srcport), List(1, f.destport))
    else List(List(2))
  def decode(index: List[Int]) = index match {
    case 2 :: xs      => "no port" -> xs
    case 0 :: p :: xs => "from port " + p -> xs
    case 1 :: p :: xs => "to port " + p -> xs
  }
}
object ProtocolIndex extends Indexing {
  def apply(f: Flow) = List(List(f.protocol))
  def decode(index: List[Int]) = ("proto " + index.head, index.tail)
}
object IPVersionIndex extends Indexing {
  def apply(f: Flow) = List(List(f.srcaddr match {
    case _: IP4Addr => 4
  }))
  def decode(index: List[Int]) = ("ipv" + index.head, index.tail)
}
sealed trait Summable {
  def apply(f: Flow): Long //TODO: a może BigInt?
}
object Bytes extends Summable {
  def apply(f: Flow) = f.bytes
}
object Packets extends Summable {
  def apply(f: Flow) = f.packets
}
object Flows extends Summable {
  def apply(f: Flow) = 1
}
object Duration extends Summable {
  def apply(f: Flow) = f.endTime - f.startTime
}

sealed trait Period {
  def apply(f: Flow): Long
  def decode(index: Long): String
}

class CheatingPeriod(val constant: Long, val decodeConstant: String) extends Period {
  def apply(f: Flow) = constant
  def decode(index: Long) = decodeConstant
}

object Always extends Period {
  def apply(f: Flow) = 0
  def decode(index: Long) = ""
}

