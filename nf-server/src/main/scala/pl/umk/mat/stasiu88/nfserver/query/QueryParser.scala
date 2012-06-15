/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query
import scala.util.parsing.combinator.RegexParsers
import pl.umk.mat.stasiu88.nfserver.Addr
import pl.umk.mat.stasiu88.nfserver.Protocols._
import pl.umk.mat.stasiu88.nfserver.Subnet
import scala.collection.JavaConversions._
import scala.collection.mutable.{Map=>MMap}
import org.joda.time._

object TimeZoneFactory{
  var cache = MMap[String,DateTimeZone]()
  for (id<-DateTimeZone.getAvailableIDs().toSet[String]){
    cache(id.toLowerCase) = DateTimeZone.forID(id)
  }
  cache("utc") = DateTimeZone.UTC
  cache("z") = DateTimeZone.UTC
  cache("default") = DateTimeZone.getDefault
  //TODO
  
  def apply(id: String):Option[DateTimeZone] = cache get id.toLowerCase
}


class QueryParser extends RegexParsers {
  
  def query: Parser[Query] = 
    timewindow ~ ";" ~ subnetdefinitions ~ ";" ~ splitfilter ~ ";" ~ statistic ^^ {
      case (tw,tz)~_~snd~_~filter~_~stat =>
        new Query(tw,tz,snd,filter,stat)
    }
    
// ---------------- TIME WINDOW

  def TIMEZONE_REGEX: Parser[String] = "[a-z/]+".r
  def timezone: Parser[DateTimeZone] = 
    TIMEZONE_REGEX ^^ {id => TimeZoneFactory(id) get } //TODO
  
  private def timezoneextractor(a: Option[Any~DateTimeZone]) = a match{
    case None => DateTimeZone.UTC
    case Some(_~t) => t
  }
  
  private def datetimeextractor(dt: ((Int,Int,Int),(Int,Int,Int)), tz: Option[Any~DateTimeZone]) = 
    new DateTime(dt._1._1, dt._1._2, dt._1._3, dt._2._1, dt._2._2, dt._2._3, timezoneextractor(tz)).getMillis()
  
  def timewindow: Parser[(TimeWindow,DateTimeZone)] = 
    datetime~"-"~datetime~opt("tz"~timezone) ^^ { case s~_~e~tz =>
      (TimeWindowBetween(datetimeextractor(s,tz), datetimeextractor(e,tz)),
          timezoneextractor(tz))
    } |
    "from" ~ datetime ~ opt("tz"~timezone) ^^ { case _~d~tz =>
      (TimeWindowAfter(datetimeextractor(d,tz)),
          timezoneextractor(tz))
    } |
    "to" ~ datetime ~ opt("tz"~timezone) ^^ { case _~d~tz =>
      (TimeWindowBefore(datetimeextractor(d,tz)),
          timezoneextractor(tz))
    } |
    "all" ~ opt("tz"~timezone) ^^ { case _~tz =>
      (AlwaysWindow, timezoneextractor(tz))
    }
    
  def datetime: Parser[((Int, Int, Int),(Int,Int,Int))] = 
    date ~ opt("t") ~ time ^^ {case a~_~b => (a,b)}
    
  def date: Parser[(Int,Int,Int)] = 
    """[0-9]{8}""".r ^^ { d => 
      (d.substring(0,4).toInt, d.substring(4,6).toInt, d.substring(6,8).toInt)
      } |
    """[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]""".r ^^ { d => 
      (d.substring(0,4).toInt, d.substring(5,7).toInt, d.substring(8,10).toInt)
      }
      
  def time: Parser[(Int,Int,Int)] = 
    """[0-9]{6}""".r ^^ { d => 
      (d.substring(0,2).toInt, d.substring(2,4).toInt, d.substring(4,6).toInt)
      } |
    """[0-9][0-9]:[0-9][0-9]:[0-9][0-9]""".r ^^ { d => 
      (d.substring(0,2).toInt, d.substring(3,5).toInt, d.substring(4,6).toInt)
      }
    
// ---------------- SUBNET DEFINITIONS

  def subnetid: Parser[String] = "[a-z]+".r
  def subnetdefinition: Parser[(String,Subnet)] = 
    subnetid ~ subnet ^^ { case id~s => (id,s)}
  def subnetdefinitions: Parser[Map[String,Subnet]] = 
    rep(subnetdefinition) ^^ {_.toMap[String,Subnet]}
  
// ---------------- FILTERS

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
    rep1sep(filterAtom, "&" | "&&" | "and") ^^ {
      case f :: Nil => f
      case x        => AndFilter(x)
    }

  def filterAtom: Parser[Filter] = "all" ^^^ AllFilter |
    "(" ~> filterExternal <~ ")" |
      ("~" | "!" | "not") ~> filterAtom ^^ { NotFilter(_) } |
      ipfilter | portfilter | protocolfilter | hostnamefilter//TODO

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
      ipdesignation ~ ( "=="|"=" |"eq") ~ ip ^^ { case d ~ _ ~ ip => IPEqualFilter(d, ip) } |
      ipdesignation ~ ("!="|"ne") ~ ip ^^ { case d ~ _ ~ ip => NotFilter(IPEqualFilter(d, ip)) } |
      ipdesignation ~ "in" ~ subnet ^^ { case d ~ _ ~ sn => IPMaskFilter(d, sn) } |
      ipdesignation ~ ("notin" | "not" ~ "in") ~ subnet ^^ { case d ~ _ ~ sn => NotFilter(IPMaskFilter(d, sn)) }
      
  def hostnamedesignation: Parser[Designation[Addr]] =
    "srchost" ^^^ SrcIpDesignation |
      ("dsthost" | "desthost") ^^^ DstIpDesignation |
      "bothhost" ^^^ BothIpDesignation |
      "anyhost" ^^^ AnyIpDesignation

  def hostname: Parser[Symbol] = """[-a-z0-9_]+(\.[-a-z0-9_]+)*""".r ^^ {Symbol(_)}
  def domainname: Parser[Symbol] = """\.?[-a-z0-9_]+(\.[-a-z0-9_]+)*""".r ^^ {Symbol(_)}
 
  def hostnamefilter: Parser[Filter] =
      hostnamedesignation ~ ( "=="|"=" |"eq") ~ hostname ^^ { case d ~ _ ~ hn => HostEqualFilter(d, hn) } |
      hostnamedesignation ~ ("!="|"ne") ~ hostname ^^ { case d ~ _ ~ hn => NotFilter(HostEqualFilter(d, hn)) } |
      hostnamedesignation ~ "in" ~ domainname ^^ { case d ~ _ ~ dn => HostInDomainFilter(d, dn) } |
      hostnamedesignation ~ ("notin" | "not" ~ "in") ~ domainname ^^ { case d ~ _ ~ dn => NotFilter(HostInDomainFilter(d, dn)) }

  def integer: Parser[Int] = """\d+""".r ^^ { _.toInt }

  def protocolfilter: Parser[Filter] =
    "tcp" ^^^ ProtocolFilter(TCP) |
      "udp" ^^^ ProtocolFilter(UDP) |
      "icmp" ^^^ ProtocolFilter(ICMP) |
      "igmp" ^^^ ProtocolFilter(IGMP) |
      "egp" ^^^ ProtocolFilter(EGP) |
      "rsvp" ^^^ ProtocolFilter(RSVP) |
      "icmp6" ^^^ ProtocolFilter(ICMP) |
      "sctp" ^^^ ProtocolFilter(SCTP) |
      "proto" ~> integer ^^ { ProtocolFilter(_) }

  def portdesignation: Parser[Designation[Int]] =
    "srcport" ^^^ SrcIntDesignation |
      ("dstport" | "destport") ^^^ DstIntDesignation |
      "anyport" ^^^ AnyIntDesignation |
      "bothport" ^^^ BothIntDesignation

  def relationaloperator: Parser[Int => (Int => Boolean)] =
    ("=="|"=" |"eq") ^^^ RelationalOperators.#==# |
      ("!="|"ne") ^^^ RelationalOperators.#!=# |
      ("<="|"le") ^^^ RelationalOperators.#<=# |
      ("<"|"lt") ^^^ RelationalOperators.#<# |
      (">="|"ge") ^^^ RelationalOperators.#>=# |
      (">"|"gt") ^^^ RelationalOperators.#>#

  def portfilter: Parser[Filter] =
    portdesignation ~ relationaloperator ~ integer ^^ { case d ~ op ~ i => PortRelationalFilter(d, op, i) } |
      portdesignation ~ "in" ~ portrange ^^ { case d ~ _ ~ ((i1,i2)) => PortRangeFilter(d, i1, i2) } |
      portdesignation ~ ("notin" | "not" ~ "in") ~ portrange ^^ { case d ~ _ ~ ((i1,i2)) => NotFilter(PortRangeFilter(d, i1, i2)) } //TODO: port op port

  def portrange: Parser[(Int,Int)] = integer ~ (".." | "...") ~ integer ^^ {case i~_~j => (i,j)}

  def interfacedesignation: Parser[Designation[Int]] =
    "inputif" ^^^ SrcIntDesignation |
      "outputif" ^^^ DstIntDesignation |
      "anyif" ^^^ AnyIntDesignation |
      "bothif" ^^^ BothIntDesignation

  def interfacefilter: Parser[Filter] =   
      hostnamedesignation ~ ( "=="|"=" |"eq") ~ integer ^^ { case d ~ _ ~ i => InterfaceEqualFilter(d, i) } |
      hostnamedesignation ~ ("!="|"ne") ~ integer ^^ { case d ~ _ ~ i => NotFilter(InterfaceEqualFilter(d, i)) }
  
  def octalDigit:Parser[Int] = ("0"|"1"|"2"|"3"|"4"|"5"|"6"|"7")^^{_.toInt}
  def tosmask: Parser[Int] = integer | "[" ~> rep1sep(octaldigit, ",") <~ "]" ^^ { _.foldl(0){ (mask,bit) => mask |= (1<<(7-bit)) } }
  
  def maskrule: Parser[Int=>Filter] = 
    ("=="|"=" |"eq") ^^^ TosEqualFilter(_) |
    "all" ^^^ TosAllFilter(_) |
    "any" ^^^ TosAnyFilter(_) |
    "none" ^^^ TosNoneFilter(_) |
    "not"~"all" ^^^ TosNotAllFilter(_)
  
  def tosfilter: Parser[Filter] = 
    "tos" ~> "bit" ~> octalDigit <~ "set" ^^ {b => TosAllFilter(1<<(7-bit))} |
    "tos" ~> "bit" ~> octalDigit <~ "clear" ^^ {b => TosNoneFilter(1<<(7-bit))} |
    "tos" ~> "bit" ~> octalDigit <~ "not" <~ "set" ^^ {b => TosNoneFilter(1<<(7-bit))} |
    "tos" ~> ("bits" | "mask") ~> tosmask <~ ("=="|"=" |"eq") ~ tosmask ^^ { case mask ~ tos => TosEqualFilter(mask, tos)} |
    "tos" ~> ("bits" | "mask") ~> tosmask ~ maskrule <~ opt("set") ^^ { case mask ~ f => f(mask) }
  
// ---------------- STATISTICS

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
              case Some(_ ~ li) => li //TODO: top
              case _            => NilIndex
            },
            t match {
              case Some(Some(i) ~ _) => i
              case _                 => 1 //TODO Int.MAX_INT ???
            },
            e getOrElse EachAlways
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
      "ip" ^^^ AnyIPIndex |
      "port" ~> "in" ~> portrange ^^ {new PortInRangeIndex(_)} |
      "port" ~> ("notin" | "not"~"in") ~> portrange ^^ {new PortNotInRangeIndex(_)} |
      "port" ^^^ AnyPortIndex |
      "srcport" ^^^ SrcPortIndex |
      ("dstport"|"destport") ^^^ DestPortIndex |
      ("protocol" | "proto") ^^^ ProtocolIndex |
      "ipversion" ^^^ IPVersionIndex |
      "host" ^^^ AnyHostnameIndex |
      "srchost" ^^^ SrcHostnameIndex |
      ("dsthost" | "desthost") ^^^ DestHostnameIndex |
      "host" ~> "in" ~> domainname ^^ {new HostnameInDomainIndex(_)} |
      "host" ~> ("notin" | "not"~"in") ~> domainname ^^ {new HostnameNotInDomainIndex(_)} |
      "tos" ~> ("bits" | "mask") ~> tosmask ^^ {new TosIndex(_)}

  def period: Parser[Period] =
    ("alltime" | "always" | ("all" ~ "time") | "all") ^^^ EachAlways |
    "dayofweek" ^^^ EachDayOfWeek |
    "hourofday" ^^^ EachHourOfDay |
    "hourofweek" ^^^ EachHourOfWeek |
    "minute" ^^^ EachMinute |
    "hour" ^^^ EachHour |
    "day" ^^^ EachDay |
    "week" ^^^ EachWeek |
    "month" ^^^ EachMonth |
    "year" ^^^ EachYear

}

object RelationalOperators{
  def #==# = new (Int=>(Int=>Boolean)){
    override def toString = "="
    def apply(i:Int) = i==_
  }
  def #!=# = new (Int=>(Int=>Boolean)){
    override def toString = "!="
    def apply(i:Int) = i!=_
  }
  def #<=# = new (Int=>(Int=>Boolean)){
    override def toString = "<="
    def apply(i:Int) = i<=_
  }
  def #>=# = new (Int=>(Int=>Boolean)){
    override def toString = ">="
    def apply(i:Int) = i>=_
  }
  def #<# = new (Int=>(Int=>Boolean)){
    override def toString = "<"
    def apply(i:Int) = i<_
  }
  def #># = new (Int=>(Int=>Boolean)){
    override def toString = ">"
    def apply(i:Int) = i>_
  }
}
