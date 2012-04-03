package pl.umk.mat.stasiu88.nfserver.manager
import scala.actors.Actor
import scala.collection.mutable
import pl.umk.mat.stasiu88.nfserver.messages._
import pl.umk.mat.stasiu88.nfserver.worker.Worker
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfserver.Logging

class Manager extends Actor with Logging{
  private[this] val cache = mutable.Map[Symbol,CacheItem]()

  val MAX_JOBS = 10
  
  def authorize(credentials: Credentials) = true //TODO
  
  def withAuthorization(credentials: Credentials)(body: =>Unit) = {
    if(authorize(credentials)){
      if(cache.size<MAX_JOBS) body
      else reply(ServiceUnavailable())
    }
    else reply(Unauthorized())
  }
  
  def act(){
    loop{
      react{
        case NewJob(q, cr) =>
          log_debug("Ordered to create new job from query "+q)
          withAuthorization(cr){
            val id = SymbolFactory.next()
            cache(id) = CacheItem(q, cr.username)
            Worker ! StartNewJob(id, q)
            reply(Accepted(id))
          }
        case GetJob(id, cr) =>
          log_debug("Asked about status of job id="+id.name)
          withAuthorization(cr){
            cache.get(id) match {
              case None => reply(NotFound())
              case Some(ci) =>
                ci.result match{
                  case None => reply(NoContent())
                  case Some(Failure(e)) => reply(ServerError(some(e)))
                  case Some(Success(r)) => reply(Ok(r, ci.originalQuery))
                }
            }
            reply(ServerError(None))
          }
        case ReportJobEnding(id, result) =>
          cache(id).result = some(result)
          cache(id).progress = 1.0
        case x => println(x)
      }
    }
  }
}
