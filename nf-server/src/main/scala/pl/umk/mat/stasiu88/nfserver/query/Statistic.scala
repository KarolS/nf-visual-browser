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


