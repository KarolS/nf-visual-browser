package pl.umk.mat.stasiu88.nfclient.chart

object PeriodType extends Enumeration {
  val MINUTE, HOUR, DAY, WEEK, MONTH, YEAR, DOW, HOW, HOD, ONE = Value
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
  def guess(period: String) = {
    part1.orElse(part2).orElse(part3).apply(period)
  }
  val part1: PartialFunction[String, Value] = {
    case "" => ONE
    case "all time" => ONE
  }
  val part2: PartialFunction[String, Value] = {
    case MINUTE_REGEX() => MINUTE
    case HOUR_REGEX() => HOUR
    case DAY_REGEX() => DAY
    case WEEK_REGEX() => WEEK
    case MONTH_REGEX() => MONTH
    case YEAR_REGEX() => YEAR
  }
  val part3: PartialFunction[String, Value] = {
    case HOD_REGEX() => HOD
    case HOW_REGEX() => HOW
    case DOW_REGEX() => DOW
    case _ => ONE //TODO: ?
  }
  def alignToTheLeft(period: String) = guess(period) match {
    case MINUTE => true
    case HOUR => true
    case HOD => true
    case HOW => true
    case _ => false
  }
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
