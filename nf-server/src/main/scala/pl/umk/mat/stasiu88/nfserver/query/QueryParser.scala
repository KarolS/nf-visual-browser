/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query
import scala.util.parsing.combinator.RegexParsers
import pl.umk.mat.stasiu88.nfserver.Addr
import pl.umk.mat.stasiu88.nfserver.Protocols._
import pl.umk.mat.stasiu88.nfserver.Subnet

class QueryParser extends RegexParsers {
  def splitfilter: Parser[SplitFilter] =
    filterExternal ~ opt(splitruleset) ^^ {
      case f ~ None    => LeafSplitFilter(f)
      case f ~ Some(r) => NodeSplitFilter(f, r)
    } | splitruleset ^^ { r => NodeSplitFilter(AllFilter, r) }

  def splitruleset: Parser[List[SplitFilter]] =
    "[" ~> rep1sep(splitfilter, ",") <~ "]"

  def filterExternal = filterAlternative |
    ("all" | "other") ^^^ AllFilter

  def filterAlternative: Parser[Filter] =
    rep1sep(filterConjunction, "|" | "||" | "or") ^^ {
      case f :: Nil => f
      case x        => OrFilter(x)
    }

  def filterConjunction: Parser[Filter] =
    rep1sep(filterInternal, "&" | "&&" | "and") ^^ {
      case f :: Nil => f
      case x        => AndFilter(x)
    }

  def filterInternal: Parser[Filter] =
    "(" ~> filterExternal <~ ")" |
      ("~" | "!") ~> filterInternal ^^ { NotFilter(_) } |
      ipfilter | portfilter | protocolfilter //TODO

  def ip: Parser[Addr] = Addr.IPV4REGEX ^^ { Addr(_) }

  def ipdesignation: Parser[Designation[Addr]] =
    "srcip" ^^^ SrcIpDesignation |
      ("dstip" | "destip") ^^^ DstIpDesignation |
      "bothip" ^^^ BothIpDesignation |
      "anyip" ^^^ AnyIpDesignation

  def subnet: Parser[Subnet] =
    ip ~ "/" ~ ip ^^ { case sn ~ _ ~ m => Subnet(sn, m) } |
      ip ~ "/" ~ integer ^^ { case sn ~ _ ~ m => Subnet(sn, m) } |
      ip ^^ { Subnet(_) } //TODO: identyfikatory

  def ipfilter: Parser[Filter] =
    ("ipv4" | "ip4") ^^^ IP4Filter |
      ("ipv6" | "ip6") ^^^ IP6Filter |
      ipdesignation ~ ("=" | "==") ~ ip ^^ { case d ~ _ ~ ip => IPEqualFilter(d, ip) } |
      ipdesignation ~ "!=" ~ ip ^^ { case d ~ _ ~ ip => NotFilter(IPEqualFilter(d, ip)) } |
      ipdesignation ~ "in" ~ subnet ^^ { case d ~ _ ~ sn => IPMaskFilter(d, sn) } |
      ipdesignation ~ ("notin" | "not" ~ "in") ~ subnet ^^ { case d ~ _ ~ sn => NotFilter(IPMaskFilter(d, sn)) }

  def integer: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def protocolfilter: Parser[Filter] =
    "tcp" ^^^ ProtocolFilter(TCP) |
      "udp" ^^^ ProtocolFilter(UDP) |
      "icmp" ^^^ ProtocolFilter(ICMP) |
      "proto" ~> integer ^^ { ProtocolFilter(_) }

  def portdesignation: Parser[Designation[Int]] =
    "srcport" ^^^ SrcIntDesignation |
      ("dstport" | "destport") ^^^ DstIntDesignation |
      "anyport" ^^^ AnyIntDesignation |
      "bothport" ^^^ BothIntDesignation

  def relationaloperator: Parser[Int => (Int => Boolean)] =
    ("=" | "==") ^^^ { (x: Int) => (y: Int) => x == y } |
      "!=" ^^^ { (x: Int) => (y: Int) => x != y } |
      "<=" ^^^ { (x: Int) => (y: Int) => x <= y } |
      "<" ^^^ { (x: Int) => (y: Int) => x < y } |
      ">=" ^^^ { (x: Int) => (y: Int) => x >= y } |
      ">" ^^^ { (x: Int) => (y: Int) => x > y }

  def portfilter: Parser[Filter] =
    portdesignation ~ relationaloperator ~ integer ^^ { case d ~ op ~ i => PortRelationalFilter(d, op, i) } |
      portdesignation ~ "in" ~ integer ~ (".." | "...") ~ integer ^^ { case d ~ _ ~ i1 ~ _ ~ i2 => PortRangeFilter(d, i1, i2) } |
      portdesignation ~ ("notin" | "not" ~ "in") ~ integer ~ (".." | "...") ~ integer ^^ { case d ~ _ ~ i1 ~ _ ~ i2 => NotFilter(PortRangeFilter(d, i1, i2)) } //TODO: port op port

  implicit def convertListOfIndexings(l: List[Indexing]): ListOfIndexing = l match {
    case Nil     => NilIndex
    case i :: xs => new ConsIndex(i, convertListOfIndexings(xs))
  }
  def statisticset: Parser[List[Statistic]] = rep1sep(statistic, ";")

  def statistic: Parser[Statistic] =
    rep1sep(summablefield, ",") ~
      opt(("top" ~> opt(integer) <~ "by") ~ rep1sep(indexingfield, ",")) ~
      opt("each" ~> period) ^^ {
        case s ~ t ~ e =>
          new Statistic(
            s,
            t match {
              case Some(_ ~ li) => li
              case _            => NilIndex
            },
            t match {
              case Some(Some(i) ~ _) => i
              case _                 => 1
            },
            e match {
              case None    => Always
              case Some(p) => p
            }
          )
      }
  def summablefield: Parser[Summable] =
    "bytes" ^^^ Bytes |
      "flows" ^^^ Flows |
      "packets" ^^^ Packets |
      "duration" ^^^ Duration

  def indexingfield: Parser[Indexing] =
    "srcip" ^^^ SrcIPIndex |
      ("dstip" | "destip") ^^^ DestIPIndex |
      "ip" ~> "in" ~> subnet ^^ { new IPInSubnetIndex(_) } |
      "ip" ~> ("notin" | "not" ~ "in") ~> subnet ^^ { new IPNotInSubnetIndex(_) } |
      "port" ^^^ PortIndex |
      ("proto" | "protocol") ^^^ ProtocolIndex |
      "ipversion" ^^^ IPVersionIndex

  def period: Parser[Period] =
    ("alltime" | "always" | ("all" ~ "time")) ^^^ Always //TODO

}