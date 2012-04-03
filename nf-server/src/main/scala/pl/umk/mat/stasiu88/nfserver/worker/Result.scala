package pl.umk.mat.stasiu88.nfserver.worker
import scala.collection.mutable.{Map=>MMap}
import scala.collection.mutable.HashMap
import pl.umk.mat.stasiu88.nfserver.query.Query
import scalaz._
import Scalaz._
class Result(private val contents: Vector[Map[Long,Map[List[Int],Long]]], val bucketCount:Int, val statisticCount:Int){
  def apply(bucket:Int, statistic:Int) = contents(bucket + statistic*bucketCount)
  
  def |+|(that: Result) = {
    require(this.bucketCount == that.bucketCount)
    require(this.statisticCount == that.statisticCount)
    val newContents = this.contents.zip(that.contents).map{t => t._1|+|t._2}
    new Result(newContents, bucketCount, statisticCount)
  }
  def toXml(q: Query)=
    <result>
    {
        for(statistic <- (0 until statisticCount)) 
        yield <statistic><type>{q.statistic.sumOver(statistic)}</type>
        {
          for(bucket <- (0 until bucketCount)) 
          yield <bucket><name>Category {bucket}</name>
          {
            for((period,m)<-apply(bucket,statistic)) 
            yield <period><timestamp>
            {
              q.statistic.period.decode(period)
              }</timestamp>{
              for((index,value)<-m) 
              yield <datapoint>
                <index>{q.statistic.indexing.decode(q, index)._1}</index>
                <value>{value}</value>
              </datapoint>
            }
            </period>
          }
          </bucket>
        }
        </statistic>
    }
    </result>
}
class MutableResult(val bucketCount:Int, val statisticCount:Int){
  private val contents = Array.fill(bucketCount*statisticCount){
    new HashMap[Long,MMap[List[Int],Long]]()
  }
  
  def apply(bucket:Int, statistic:Int) = contents(bucket + statistic*bucketCount)
  def add(bucket:Int, statistic:Int, period:Long, index: List[Int], value:Long) {
    val m = apply(bucket,statistic)
    if(!m.contains(period)) m(period) = new HashMap()
    val mm = m(period)
    if(!mm.contains(index)) mm(index)=value
    else mm(index)=mm(index)+value
  }
  def freeze():Result = {
    new Result(
        Vector(contents.view.map{ m:HashMap[Long,MMap[List[Int],Long]]=>
            Map(m.view.map{ case (i:Long,mm:HashMap[List[Int],Long])=>
              i->Map(mm.view.toSeq:_*)
            }.toSeq:_*)
        }.toSeq:_*),
        bucketCount,
        statisticCount
    )
  }
}