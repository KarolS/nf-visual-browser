package pl.umk.mat.stasiu88.nfserver.datasource
import scala.collection.mutable.HashMap
import scala.xml.XML
import scala.xml.NodeSeq
import java.net.URL
import java.io.File
import java.io.FileInputStream
import pl.umk.mat.stasiu88.nfserver.Logging

object FileRangeCache extends Logging {
  private[this] val cache = new HashMap[String,(Long,Long)].withDefaultValue((Long.MinValue, Long.MaxValue))
  private[this] var xmlFilename:String = null
  
  def load(filename: String){
    synchronized {
      this.xmlFilename = filename
      cache.clear()
      try{
        val x = XML.load(new FileInputStream(filename))
        for(node <- x \ "cache"){
            cache(node \ "@filename" text) = (
              (node \ "@from" text).toLong,
              (node \ "@to" text).toLong
            )
        }
      } catch {
        case _ =>
          log_warn("File "+xmlFilename+" cannot be read, assuming empty cache")
      }
    }
  }
  def update(filename: String, firstSeen: Long, lastSeen: Long){
    synchronized {
      cache(filename) = (firstSeen, lastSeen)
    }
  }
  
  def get(filename: String) = cache(filename)
  
  def commit(){
    synchronized {
      if(xmlFilename == null){
        log_info("Commiting skipped: no cache file specified")
      }
      else{
        val xmllist = cache.map{ case (filename,(from,to)) =>
          <file name={filename} from={from.toString} to={to.toString}/>
        }.toSeq
        val xml = <cache>{NodeSeq.fromSeq(xmllist)}</cache>
        try {
          XML.save(xmlFilename, xml)
        } catch {
          case _ =>
            log_error("File cache commiting failed")
        }
      }
    }
  }
}