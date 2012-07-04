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
 * Statistic
 * <br>
 * Statystyka
 */
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

/**
 * Represents a summable field of a flow record.
 * <br>
 * Reprezentuje sumowalne pole w rekordzie przepływu.
 */
sealed trait Summable {
  /**
   * Extracts value of a field from the flow record
   * <br>
   * Wyciąga wartość pola z rekordu przepływu.
   */
  def apply(f: Flow): Long //TODO: a może BigInt?
}

/**
 * Represents a field of number of bytes in a flow record.
 * <br>
 * Reprezentuje pole rekordu przepływu z liczbą bajtów.
 */
case object Bytes extends Summable {
  def apply(f: Flow) = f.bytes
}
/**
 * Represents a field of number of packets in a flow record.
 * <br>
 * Reprezentuje pole rekordu przepływu z liczbą pakietów.
 */
case object Packets extends Summable {
  def apply(f: Flow) = f.packets
}
/**
 * Represents a number of flows in a flow record (= 1).
 * <br>
 * Reprezentuje liczbę przepływów w rekordzie z przepływem (= 1).
 */
case object Flows extends Summable {
  def apply(f: Flow) = 1
}
/**
 * Represents duration of the flow.
 * <br>
 * Reprezentuje czas trwania przepływu.
 */
case object Duration extends Summable {
  def apply(f: Flow) = f.endTime - f.startTime
}


