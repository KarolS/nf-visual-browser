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
 * Assigns a period to each flow.
 * <br>
 * Przypisuje przepływowi okres. 
 */
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

/**
 * Assigns to each flow the same period.
 * <br>
 * Przypisuje każdemu przepływowi ten sam okres.
 */
case object EachAlways extends Period {
  def apply(q:Query, f: Flow) = 0L
  def decode(index: Long) = "all time"
}

/**
 * Assigns to a flow the minute when it ended.
 * <br>
 * Przypisuje przepływowi minutę, w której się skończył.
 */
case object EachMinute extends Period {
  val FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  def apply(q:Query, f: Flow) = f.endTime / 60000L
  def decode(index: Long) = new DateTime(EPOCH.plus(index * (60*1000))).toString(FORMAT)
}

/**
 * Assigns to a flow the hour when it ended.
 * <br>
 * Przypisuje przepływowi godzinę, w której się skończył.
 */
case object EachHour extends Period {
  val FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd HH")
  def apply(q:Query, f: Flow) = 
    Hours.hoursBetween(EPOCH, new Instant(f.endTime)).getHours().toLong
    
  def decode(index: Long) = new DateTime(EPOCH.plus(index * (3600*1000))).toString(FORMAT)
}

/**
 * Assigns to a flow the day when it ended.
 * <br>
 * Przypisuje przepływowi dzień, w którym się skończył.
 */
case object EachDay extends Period {
  val FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd")
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).dayOfMonth().roundFloorCopy().getMillis() / (24L*3600*1000)
    
  def decode(index: Long) = new DateTime(EPOCH.plus(index * (24L*3600*1000))).toString(FORMAT)
}

/**
 * Assigns to a flow the week when it ended.
 * <br>
 * Przypisuje przepływowi tydzień, w którym się skończył.
 */
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

/**
 * Assigns to a flow the month when it ended.
 * <br>
 * Przypisuje przepływowi miesiąc, w którym się skończył.
 */
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

/**
 * Assigns to a flow the year when it ended.
 * <br>
 * Przypisuje przepływowi rok, w którym się skończył.
 */
case object EachYear extends Period {
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).getYear()
    
  def decode(index: Long) = index.toString
}

/**
 * Assigns to a flow the day of the week when it ended.
 * <br>
 * Przypisuje przepływowi dzień tygodnia, w którym się skończył.
 */
case object EachDayOfWeek extends Period {
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).getDayOfWeek()
    
  def decode(index: Long) = DOW(index.toInt)
}

/**
 * Assigns to a flow the hour of the day when it ended.
 * <br>
 * Przypisuje przepływowi godzinę doby, w której się skończył.
 */
case object EachHourOfDay extends Period {
  def apply(q:Query, f: Flow) = 
    new DateTime(f.endTime, q.timeZone).getHourOfDay()
    
  def decode(index: Long) = if(index<10) "0"+index else index.toString
}

/**
 * Assigns to a flow the hour of the week when it ended.
 * <br>
 * Przypisuje przepływowi godzinę tygodnia, w której się skończył.
 */
case object EachHourOfWeek extends Period {
  def apply(q:Query, f: Flow) = { 
    val x = new DateTime(f.endTime, q.timeZone)
    x.getHourOfDay() + 24*x.getDayOfWeek()
  }
  
  def decode(index: Long) = DOW(index.toInt/24)+(if(index%24<10) "-0"+index%24 else "-"+index%24)
}


