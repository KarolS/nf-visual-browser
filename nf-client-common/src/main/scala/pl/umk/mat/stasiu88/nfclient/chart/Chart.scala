package pl.umk.mat.stasiu88.nfclient.chart
import scala.xml.Node
import pl.umk.mat.stasiu88.nfclient.Utils._
import pl.umk.mat.stasiu88.nfclient.logging.Logging
import org.joda.time.DateTime
import scala.collection.mutable.ArrayBuilder
import scala.collection.mutable.ArrayBuffer
import org.joda.time.format.DateTimeFormat
trait Chart
object Chart {
  
  val HOD_A = (0 to 23).map { twoDigit _ }.toArray
  
  val DOW_A = Array("MON","TUE","WED","THU","FRI","SAT","SUN")
  val HOW_A = (for(d<-DOW_A; h<-HOD_A) yield d+"-"+h).toArray
  
  def twoDigit(i:Int) = if(i<10) "0"+i else ""+i
  
  val DAYS_FORMATTER = DateTimeFormat.forPattern("yyyy-MM-dd")
  val WEEK2_FORMATTER = DateTimeFormat.forPattern("MM-dd")
  val HOURS_FORMATTER = DateTimeFormat.forPattern("yyyy-MM-dd HH")
  val MINUTES_FORMATTER = DateTimeFormat.forPattern("yyyy-MM-dd HH:mm")
  def continuousPeriods(somePeriods: Set[String]):Array[String] = {
    val min = somePeriods.min
    val max = somePeriods.max
    import PeriodType._
    try{
      guess(min) match {
        case ONE => somePeriods.toArray.sortWith(_<_)
        case HOD => HOD_A
        case DOW => DOW_A
        case HOW => HOW_A
        case YEAR => (min.toInt to max.toInt).map{_.toString}.toArray
        case MONTH => (
            for(y <- (min.split("-")(0).toInt to max.split("-")(0).toInt);
                m <- (1 to 12)map{twoDigit _};
                v = y+"-"+m;
                if min<=v && v<=max) yield v
        ).toArray
        case DAY =>
          val startDay = DateTime.parse(min, DAYS_FORMATTER)
          val buffer = new ArrayBuffer[String]
          var now = startDay
          var nows = now.toString(DAYS_FORMATTER)
          while(nows<=max){
            buffer += nows
            now = now.plusDays(1)
            nows = now.toString(DAYS_FORMATTER)
          }
          buffer.toArray
        case HOUR =>
          val startDay = DateTime.parse(min, HOURS_FORMATTER)
          val buffer = new ArrayBuffer[String]
          var now = startDay
          var nows = now.toString(HOURS_FORMATTER)
          while(nows<=max){
            buffer += nows
            now = now.plusHours(1)
            nows = now.toString(HOURS_FORMATTER)
          }
          buffer.toArray
        case MINUTE =>
          val startDay = DateTime.parse(min, MINUTES_FORMATTER)
          val buffer = new ArrayBuffer[String]
          var now = startDay
          var nows = now.toString(MINUTES_FORMATTER)
          while(nows<=max){
            buffer += nows
            now = now.plusMinutes(1)
            nows = now.toString(MINUTES_FORMATTER)
          }
          buffer.toArray
        case WEEK =>
          val startDay = DateTime.parse(min.split("/")(0),DAYS_FORMATTER)
          val buffer = new ArrayBuffer[String]
          var now = startDay
          var nows = now.toString(DAYS_FORMATTER)+"/"+now.plusDays(6).toString(WEEK2_FORMATTER)
          while(nows<=max){
            buffer += nows
            now = now.plusDays(7)
            nows = now.toString(DAYS_FORMATTER)+"/"+now.plusDays(6).toString(WEEK2_FORMATTER)
          }
          buffer.toArray
        case _ => somePeriods.toArray.sortWith(_<_)
      }
    }catch {
      case _=> somePeriods.toArray.sortWith(_<_)
    }
    
  }
  def dowIdentifier(n:String) = n.substring(0,2).toLowerCase match {
    case "mo" => Some(1)
    case "tu" => Some(2)
    case "we" => Some(3)
    case "th" => Some(4)
    case "fr" => Some(5)
    case "sa" => Some(6)
    case "su" => Some(7)
    case _ => None
  }
  def periodSorter(p1:String, p2:String):Boolean = { 
    if(p1=="" || p2=="") return p1<p2
    if(p1.charAt(0)<'A' || p2.charAt(0)<'A') return p1<p2
    val s1 = p1.split(" ")
    val s2 = p2.split(" ")
    if(s1.length>2 || s1.length<1 || s1.length!=s2.length) return p1<p2
    if(s1.length==1){
      val t1 = p1.split("-")
      val t2 = p2.split("-")
      if(s1.length>2 || s1.length<1 || s1.length!=s2.length) return p1<p2
      if(t1.length == 1){
        dowIdentifier(p1) -> dowIdentifier(p2) match {
          case (Some(i), Some(j)) => i<j
          case _ => p1<p2
        }
      }else{
        dowIdentifier(t1(0)) -> dowIdentifier(t2(0)) match {
          case (Some(i), Some(j)) => if (i==j) t1(1)<t2(1) else i<j 
          case _ => p1<p2
        }
      }
    }
    else{
      dowIdentifier(s1(0)) -> dowIdentifier(s2(0)) match {
        case (Some(i), Some(j)) => if (i==j) s1(1)<s2(1) else i<j 
        case _ => p1<p2
      }
    }
  }
  
  def periodMapper[A]
      (periods: Array[String],periods2: Seq[Period], defaultValue: A)
      (f:Period=>A)
      (implicit manifest: Manifest[A]) = {
    val result = Array.fill(periods.length)(defaultValue)
    val index = scala.collection.mutable.Map[String,Int]()
    for((p,i)<-periods zipWithIndex){
      index(p)=i
    }
    for(p<-periods2){
      result(index(p.timestamp)) = f(p)
    }
    result
  }
  def getIndexCount(statistics: List[Statistic]):Int = {
    for(s<-statistics; b<-s.buckets; p<-b.periods){
      if(p.datapoints.size>1) return 2
      if(p.datapoints.size==1 && p.datapoints(0).index=="") return 0
    }
    return 1
  } 
  
  def getPeriodCount(statistics: List[Statistic]):Int = {
    for(s<-statistics; b<-s.buckets){
      if(b.periods.size>1) return 2
    }
    return 1
  }
  
  def convertBucket(bucket: Bucket, indexPeriodCount: (Int,Int)): Chart = {
    if(bucket.periods.size==0){
      new EmptyChart("No data")
    } else {
      indexPeriodCount match {
        case (0,1) => 
          if(bucket.periods.head.datapoints.size==0) new EmptyChart("No data")
          else new Table2(Array(""), Array(bucket.periods(0).datapoints(0).value))
        case (1,1) => 
          if(bucket.periods.head.datapoints.size==0) new EmptyChart("No data")
          else new Table2(
            Array(bucket.periods.head.datapoints.head.index), 
            Array(bucket.periods.head.datapoints.head.value)
          )
        case (2,1) =>
          if(bucket.periods.head.datapoints.size==0) new EmptyChart("No data")
          else {
            val indexes = bucket.periods.head.datapoints.map{_.index}.toArray
            val values = bucket.periods.head.datapoints.map{_.value}.toArray
            new Table2(indexes,values)
          }
        case (0,2) =>
          //val periods = bucket.periods.map{_.timestamp}.sortWith(periodSorter _).toArray
          val periods = continuousPeriods(bucket.periods.map{_.timestamp}.toSet)
          val values = periodMapper(periods,bucket.periods,0L)(_.datapoints.head.value)
          new GraphableTable2(periods,values)
        case (1,2) =>
          val periods = bucket.periods.map{_.timestamp}.sortWith(periodSorter _).toArray
          val indexes = periodMapper[String](periods,bucket.periods,null)(_.datapoints.headOption.map{_.index}.orNull)
          val values = periodMapper(periods,bucket.periods,0L)(_.datapoints.headOption.map{_.value}.getOrElse(0L))
          new Table3(periods, indexes, values)
        case (2,2) =>
          val periods = bucket.periods.sortWith{(p1, p2)=>
            periodSorter (p1.timestamp,p2.timestamp)
          }.toArray
          val triples = for(period<-periods; datapoint<-period.datapoints) yield (period.timestamp,datapoint.index,datapoint.value)
          new Table3(
            triples.map{_._1}.toArray,
            triples.map{_._2}.toArray,
            triples.map{_._3}.toArray
          )
        case _ =>
          new EmptyChart("Error")
      }
    }
  }
  
  def createPieChart(statistic: Statistic) = {
    val nonEmptybuckets = statistic.buckets.filter{_.periods.isEmpty == false}
    val categories = nonEmptybuckets.map{_.name}.toArray
    val values = nonEmptybuckets.map{_.periods.head.datapoints.head.value}.toArray
    new PieChart(categories, values)
  }
  
  // TODO improve support for buckets with non-equal period lists
  def createCumulativeChart(statistic: Statistic) = {
    val categories = statistic.buckets.map{_.name}.toArray
    if(categories.length == 0) null
    else{
      /*val periods = (
        statistic.buckets
        .map{_.periods.map{_.timestamp}.toSet}
        .foldLeft(Set[String]())(_++_)
        .toArray
        .sortWith(periodSorter _)
        .toArray
      )*/
      val periods = continuousPeriods(
        statistic.buckets
        .map{_.periods.map{_.timestamp}.toSet}
        .foldLeft(Set[String]())(_++_)
      )
      if(periods.size == 0) null
      else{
        val values = statistic.buckets.map{ b =>
          periodMapper(periods, b.periods, 0L){ p =>
            p.datapoints.head.value
          }
        }.toArray
        new CumulativeChart(categories, periods, values)
      }
    }
  }
  
  def convertStatistic(statistic: Statistic, indexPeriodCount: (Int,Int)): StatChart = {
    new StatChart(
      statistic.typ, 
      statistic.buckets.map{ b => 
        new CategoryChart(b.name, convertBucket(b, indexPeriodCount))
      },
      if(statistic.buckets.length>1){
        indexPeriodCount match{
          case (0,1) => 
            Option(createPieChart(statistic))
          case (0,2) =>
            Option(createCumulativeChart(statistic))
          case _ => 
            None
        }
      } else {
        None
      }
    )
  }
  
  def convertStatistics(statistics: List[Statistic]) = {
    val indexCount = getIndexCount(statistics)
    val periodCount = getPeriodCount(statistics)
    statistics map {convertStatistic(_, (indexCount,periodCount))}
  }
  
}

class CategoryChart(val category: String, val chart:Chart)
class StatChart(
  val statistic: String, 
  val categoryCharts: List[CategoryChart], 
  val crossCategoryChart: Option[Chart]
  ){
  @deprecated(message="temporary", since="always")
  def toChartList = crossCategoryChart + categoryCharts
}
