package pl.umk.mat.stasiu88.nfclient.agent

import pl.umk.mat.stasiu88.nfclient.ui.UI
import pl.umk.mat.stasiu88.nfclient.ui.UIComponent
import pl.umk.mat.stasiu88.nfclient.SHttpClient
import pl.umk.mat.stasiu88.nfclient.messages._
import scalaz._
import Scalaz._
import scala.collection.mutable
import pl.umk.mat.stasiu88.nfclient.logging.Logging
import pl.umk.mat.stasiu88.nfclient.logging.ConsoleLogging
import pl.umk.mat.stasiu88.nfclient.Utils._
import pl.umk.mat.stasiu88.nfclient.chart.XmlReader

trait DefaultAgentComponent extends AgentComponent {
  this: UIComponent =>
  lazy val agent = new DefaultAgent with ConsoleLogging
}
class DefaultAgent extends Agent{
  this: Logging => //TODO: better logging facilities
  private [this] var ui: UI = null
  private [this] var server = "http://localhost:80"
  private [this] var username = "user"
  private [this] var password = "pass"
  
  private [this] val client = new SHttpClient()
  def init(ui: UI) {
    log_debug("Got a UI")
    this.ui = ui
  }
  
  private [this] val cache = mutable.Map[Symbol,CacheItem]().withDefault(_=>NonExistentItem)
  
  def cacheItem(id: Symbol) = synchronized {cache(id)}
  
  def doNewQuery(query: String) {
    try{
      val (statusCode, Left(result)) = client.request(server+"/new", 
        Map( "username" -> username,
          "password" -> password,
          "query" -> query
        )
      )(_ => false)
      log_debug("Status code when submitting: "+statusCode)
      statusCode match {
        case 202 => 
          result match {
            case Failure(f) => ui ! ServerError(f)
            case Success(id) =>
              //TODO: Something about cache
              val sid = Symbol(id)
              synchronized{
                cache(sid) = FreshItem
              }
              ui ! JobAccepted(sid)
          }
        case 401 => ui ! InvalidCredentials
        case 503 => ui ! ServerBusy
        case i => ui ! ServerError(
          i.toString
          + "\n"
          + result.fold(_ toString, i=>i)
        )
      } 
    } catch {
      case e:Exception =>
        ui ! ServerNotResponding
    }
  }
  
  def doCancelQuery(id: Symbol){
    log_trace("Cancelling query id="+id.name)
    try{
      val (statusCode, stringOrXml) = client.request(server+"/cancel", 
        Map( "username" -> username,
          "password" -> password,
          "id" -> id.name
        )
      )(_=>false)
      
      log_debug("Status code when cancelling: "+statusCode)
      
      statusCode match {
        case 202 => 
          synchronized{
            cache(id) = CancelledItem
          }
          ui ! JobCancelled(id)
        case 404 => 
          synchronized{
            cache(id) = NonExistentItem
          }
          ui ! JobInterrupted(id)
        case 401 => 
          ui ! InvalidCredentials
        case 503 => 
          ui ! JobUnknown(id)
        case i => 
          val err = i.toString +
            "\n" +
            stringOrXml.fold(_ fold(_ toString, i=>i), _ fold(_ toString, _ toString))
          synchronized{
            cache(id) = FailedItem(err)
          }
          ui ! JobError(
            id,
            err
          )
      }
    } catch {
      case e:Exception =>
        log_error("Fail")
        ui ! ServerNotResponding
    }
  }
  def doRefreshQuery(id: Symbol, beAnnoying: Boolean){
    log_trace("Refreshing query id="+id.name)
    try{
      val (statusCode, stringOrXml) = client.request(server+"/get", 
        Map( "username" -> username,
          "password" -> password,
          "id" -> id.name
        )
      )(_ == 200)
      log_debug("Status code when refreshing: "+statusCode)
      statusCode match {
        case 204 => ui ! JobInProgress(id)
        case 206 => 
          try{
            synchronized{
              cache(id) = ItemInProgress(stringOrXml.fold(_ fold(_ toString, i=>i), _ fold(_ toString, _ toString)).toDouble)
            }
            ui ! JobInProgress(id)
          } catch {
            case e:Exception => 
              e.printStackTrace()
              println(stringOrXml)
          }
        case 200 =>
          val Right(xml_?) = stringOrXml
          //log_debug(xml.toString)
          xml_? match {
            case Success(xml) =>
              val result = XmlReader read xml
              //log_debug(result.toString)
              synchronized{
                cache(id) = SuccessfulItem(result)
              }
              ui ! JobDone(id)
            case Failure(f) =>
              synchronized{
                cache(id) = FailedItem(f)
              }
              ui ! JobError(id, f)
          }
          
        case 404 => 
          synchronized{
            cache(id) = NonExistentItem
          }
          ui ! JobInterrupted(id)
        case 401 => ui ! InvalidCredentials
        case 503 => ui ! JobUnknown(id)
        case i =>
          val err = i.toString +
            "\n" +
            stringOrXml.fold(_ fold(_ toString, i=>i), _ fold(_ toString, _ toString))
          synchronized{
            cache(id) = FailedItem(err)
          }
          ui ! JobError(
            id,
            err
          )
      }
    } catch {
      case e:Exception =>
        log_error("Fail")
        if(beAnnoying) ui ! ServerNotResponding
    }
  }
  def act() = loop {
    log_trace("Agent waits for a message")
    try {
      react {
        case SetCredentials(u,p) =>
          log_debug("Setting credentials "+(u,p))
          username = u
          password = p
        case SetServer(s) =>
          log_debug("Setting server "+s)
          server = s
        case NewQuery(query) =>
          log_debug("Submitting "+query)
          doNewQuery(query)
        case RefreshQuery(id) =>
          log_debug("Refreshing "+id.name)
          doRefreshQuery(id, true)
        case CancelQuery(id) =>
          log_debug("Refreshing "+id.name)
          doCancelQuery(id)
        case RefreshAllQueries =>
          val ids = synchronized {
            cache.filter{case(k,v)=> v.isRefreshable}.map{_._1}
          }
          log_debug("Refreshing all "+ids.size+" queries")
          for (id <- ids) doRefreshQuery(id, false)
        case _ =>
          log_warn("Unknown message type")
      }
    } catch {
      case ie: InterruptedException =>
        throw ie
      case e:Exception =>
        log_error(e.toString)
        e.printStackTrace()
    }
  }
}