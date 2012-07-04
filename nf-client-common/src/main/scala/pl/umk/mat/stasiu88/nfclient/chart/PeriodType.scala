/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.chart

/**
 * Enumerationm for period types.
 * <br>
 * Typ wyliczeniowy z typami okresów. 
 */
object PeriodType extends Enumeration {
  val MINUTE, HOUR, DAY, WEEK, MONTH, YEAR, DOW, HOW, HOD, ONE = Value
  /**
   * Returns a solidus + unit name, if applicable.
   * <br>
   * Zwraca ukośnik + nazwę jednostki, o ile dotyczy.
   */
  def per(v: Value) = v match{
    case MINUTE => "/min"
    case HOUR => "/h"
    case DAY => "/day"
    case WEEK => "/week"
    case MONTH => "/month"
    case YEAR => "/year"
    case _ => ""
  }
  val MINUTE_REGEX = "[0-9]+-[0-9]+-[0-9]+ [0-9]+:[0-9]+"r
  val HOUR_REGEX = "[0-9]+-[0-9]+-[0-9]+ [0-9]+"r
  val DAY_REGEX = "[0-9]+-[0-9]+-[0-9]+"r
  val WEEK_REGEX = "[0-9]+-[0-9]+-[0-9]+/[0-9]+-[0-9]+"r
  val MONTH_REGEX = "[0-9]+-[0-9]+"r
  val YEAR_REGEX = "[0-9][0-9][0-9]+"r
  val HOD_REGEX = "[0-9][0-9]?"r
  val DOW_REGEX = "[A-Za-z]+"r
  val HOW_REGEX = "[A-Za-z]+-[0-9]+"r
  
  //This one is split into parts due to compiler bug
  /**
   * Guesses a period type based on a period string representation.
   * <br>
   * Zgaduje typ okresu na postawie łańcucha.
   */
  def guess(period: String) = {
    part1.orElse(part2).orElse(part3).apply(period)
  }
  private[this] val part1: PartialFunction[String, Value] = {
    case "" => ONE
    case "all time" => ONE
  }
  private[this] val part2: PartialFunction[String, Value] = {
    case MINUTE_REGEX() => MINUTE
    case HOUR_REGEX() => HOUR
    case DAY_REGEX() => DAY
    case WEEK_REGEX() => WEEK
    case MONTH_REGEX() => MONTH
    case YEAR_REGEX() => YEAR
  }
  private[this] val part3: PartialFunction[String, Value] = {
    case HOD_REGEX() => HOD
    case HOW_REGEX() => HOW
    case DOW_REGEX() => DOW
    case _ => ONE //TODO: ?
  }
  /**
   * Whether the label with period name should be under the left edge of the bar instead of the middle.
   * <br>
   * Czy etykieta z nazwą okresu powinna być pod lewym krańcem słupka zamiast pod jego środkiem.
   */
  def alignToTheLeft(period: String) = guess(period) match {
    case MINUTE => true
    case HOUR => true
    case HOD => true
    case HOW => true
    case _ => false
  }
  /**
   * Whether render or not a label, for a given bar width.
   * <br>
   * Czy renderować etykietę, czy nie, dla podanej szerokości słupka.
   */
  def isImportant(typ: Value, period:String, periodWidth:Double):Boolean ={
    typ match {
      case MINUTE => 
        if(periodWidth>50) true
        else if(periodWidth>1) period.endsWith("00")
        else if(periodWidth*60>1) period.endsWith("00:00")
        else if(periodWidth*1800>1) period.endsWith("01 00:00")
        else period.endsWith("01-01 00:00")
      case HOUR => 
        if(periodWidth>50) true 
        else if(periodWidth>2) period.endsWith("00")
        else if(periodWidth*30>2) period.endsWith("01 00")
        else period.endsWith("01-01 00")
      case DAY => 
        if(periodWidth>50) true 
        else if(periodWidth>2) period.endsWith("01")
        else period.endsWith("01-01")
      case HOD => 
        if(periodWidth>20) true
        else period.toInt % 6 == 0
      case HOW => 
        if(periodWidth>30) true
        else period.endsWith("00")
      case _ => true
    }
  }
}
