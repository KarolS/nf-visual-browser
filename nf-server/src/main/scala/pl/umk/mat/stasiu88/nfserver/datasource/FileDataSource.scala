package pl.umk.mat.stasiu88.nfserver.datasource

import java.io.File

import scala.collection.breakOut

import pl.umk.mat.stasiu88.nfserver.input.DataFile
import pl.umk.mat.stasiu88.nfserver.query.QueryModel._
import pl.umk.mat.stasiu88.nfserver.query._
import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.Flow
import scalaz._
import Scalaz._

class FileDataSource(val file: File) extends DataSource{
  val filename = file.getAbsolutePath()
  def this(name: String) = this(new File(name))
  
  @volatile
  private[this]var vfirstSeen = Long.MinValue
  @volatile
  private[this]var vlastSeen = Long.MaxValue
  @volatile
  private[this]var read = false
  @volatile
  private[this]var assumeValid = true
  def firstSeen = vfirstSeen
  def lastSeen = vlastSeen
  
  private def readHeader(q:Query):Option[DataFile]={
    if(assumeValid && !read && q.timeWindow.overlapsWith(firstSeen,lastSeen)){
      synchronized {
        try{
          val data = new DataFile(filename)
          val vfirstSeen = data.firstSeen
          val vlastSeen = data.lastSeen
          read = true
          return Some(data)
        }catch{
          case _ =>
            assumeValid = false
            return None
        }
      }
    }
    None
  }
  override def getQuickResult(q:Query):Option[Result]={
    //TODO: optimize for pure TCP/UDP/ICMP queries
    val data = readHeader(q) match {
      case Some(d) => d
      case None => return None
    }
    if(!assumeValid) return None
    if(!q.timeWindow.contains(firstSeen,lastSeen)) return None
    if(q.splitfilter /== LeafSplitFilter(AllFilter)) return None
    if(q.statistic.backupPeriod /== EachAlways) return None
    if(q.statistic.indexing /== NilIndex) return None
    q.statistic.sumOver foreach{ 
      case Bytes => ()
      case Packets => ()
      case Flows => ()
      case _ => return None
    }
    try{
      val flows = data.flowCount
      val bytes = data.byteCount
      val packets = data.packetCount
      some(new Result(Vector(q.statistic.sumOver.map{
        case Bytes => Map(0L->Map(List[Int]()->bytes))
        case Packets => Map(0L->Map(List[Int]()->packets))
        case Flows => Map(0L->Map(List[Int]()->flows))
        case _ => return None
      }:_*), 1, q.statistic.sumOver.length))
    } catch {
      case e => None
    }
    finally{
      try{
        data.close()
      } catch {case _ => }
    }
  }
  
  def foreach(q: Query)(f: Flow=>Unit){
    readHeader(q) map {data =>
      if(assumeValid && q.timeWindow.overlapsWith(firstSeen,lastSeen)){
        data foreach f
      }
      try{
        data.close()
      } catch {case _ => }
    }
  }
}