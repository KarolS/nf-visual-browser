/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver.Subnet
import org.joda.time._

sealed trait TimeWindow{
  def contains(l: Long):Boolean
  def overlapsWith(w: TimeWindow): Boolean = overlapsWith(w.approxStart, w.approxEnd)
  def overlapsWith(start: Long, end:Long): Boolean
  def contains(start:Long, end: Long): Boolean
  def approxStart:Long
  def approxEnd:Long
  def unary_~ : TimeWindow
}
case object AlwaysWindow extends TimeWindow{
  def contains(l:Long) = true
  override def overlapsWith(w: TimeWindow) = true
  def overlapsWith(start: Long, end:Long) = true
  def contains(start:Long, end: Long) = true
  def approxStart = Long.MinValue
  def approxEnd = Long.MaxValue
  def unary_~ = NeverWindow
}
case object NeverWindow extends TimeWindow{
  def contains(l:Long) = false
  override def overlapsWith(w: TimeWindow) = false
  def overlapsWith(start: Long, end:Long) = false
  def contains(start:Long, end: Long) = false
  def approxStart = Long.MaxValue
  def approxEnd = Long.MinValue
  def unary_~ = AlwaysWindow
}
case class TimeWindowBefore(x: Long) extends TimeWindow{
  def contains(l:Long) = l<x
  def overlapsWith(start: Long, end:Long) = start<x
  def contains(start:Long, end: Long) = end<=x
  def approxStart = Long.MinValue
  def approxEnd = x
  def unary_~ = TimeWindowAfter(x)
}
case class TimeWindowAfter(x: Long) extends TimeWindow{
  def contains(l:Long) = l>=x
  def overlapsWith(start: Long, end:Long) = end>x
  def contains(start:Long, end: Long) = start>=x
  def approxStart = x
  def approxEnd = Long.MaxValue
  def unary_~ = TimeWindowBefore(x)
}
case class TimeWindowBetween(a: Long, b:Long) extends TimeWindow{
  def contains(l:Long) = (l>=a)&&(l<b)
  def overlapsWith(start: Long, end:Long) = end>a || start<b
  def contains(start:Long, end: Long) = start>=a && end<=b
  def approxStart = a
  def approxEnd = b
  def unary_~ = TimeWindowOutside(a, b)
}
case class TimeWindowOutside(a: Long, b:Long) extends TimeWindow{
  def contains(l:Long) = (l<a)||(l>=b)
  def overlapsWith(start: Long, end:Long) = start<a || end >b
  def contains(start:Long, end: Long) = start>=b || end<=a
  def approxStart = Long.MinValue
  def approxEnd = Long.MaxValue
  def unary_~ = TimeWindowBetween(a, b)
}

object Query{
  val parser = new QueryParser
  def apply(s: String):Query = parser.parseAll(parser.query,s).get
}
class Query(
    val timeWindow: TimeWindow,
    val timeZone: DateTimeZone,
    val subnets: Map[String,Subnet],
    non_pure_splitfilter: SplitFilter,
    val statistic: Statistic
    ){
  
  val splitfilter = non_pure_splitfilter.replaceSubnets(subnets)
  splitfilter.bucketNo = 0
  
  splitfilter.name("")
  
  def clipSize = statistic.top
  def needsClipping = statistic.indexing != NilIndex
}