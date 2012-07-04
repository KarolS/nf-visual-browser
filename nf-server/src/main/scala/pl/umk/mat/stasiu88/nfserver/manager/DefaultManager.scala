/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.manager
import scala.actors.Actor
import scala.collection.mutable
import pl.umk.mat.stasiu88.nfserver.messages._
import pl.umk.mat.stasiu88.nfserver.worker.Worker
import scalaz._
import Scalaz._
import pl.umk.mat.stasiu88.nfserver.Logging
import pl.umk.mat.stasiu88.nfserver.authz.AuthzSource
import pl.umk.mat.stasiu88.nfserver.worker.WorkerComponent
import pl.umk.mat.stasiu88.nfserver.authz.AuthzSourceComponent
import org.joda.time.Instant

trait DefaultManagerComponent extends ManagerComponent{
  this: WorkerComponent with AuthzSourceComponent =>
  lazy val manager = new DefaultManager 
}

/**
 * Default manager
 * <br>
 * Domyślny menedżer
 */
class DefaultManager extends Manager with Actor with Logging {
  private[this] val cache = mutable.Map[Symbol,CacheItem]()

  val MAX_JOBS = 10
  
  private[this] var authSource:AuthzSource = null
  private[this] var worker:Worker = null
  
  def init(authSource: AuthzSource, worker: Worker){
    this.authSource = authSource
    this.worker = worker
  }
    
  private def withAuthorization(credentials: Credentials)(body: =>Unit) = {
    if(authSource.authorize(credentials)){
      if(cache.filterNot{
        case (id, ci) => ci.isFinished
      }.size<MAX_JOBS) body // TODO ?
      else reply(ServiceUnavailable())
    }
    else reply(Unauthorized())
  }
  
  def act(){
    loop{
      try{
        var toRemove = Set[Symbol]()
        val NOW = Instant.now()
        cache.foreach{
          case (id,item) =>
            if(item.timeout.compareTo(NOW)<0 && item.isFinished) toRemove += id
        }
        toRemove foreach {cache remove _}
        react{
          case NewJob(q, cr) =>
            log_debug("Ordered to create new job from query "+q)
            withAuthorization(cr){
              val id = SymbolFactory.next()
              cache(id) = CacheItem(q, cr.username)
              worker ! StartNewJob(id, q)
              reply(Accepted(id))
            }
          case GetJob(id, cr) =>
            log_debug("Asked about status of job id="+id.name)
            withAuthorization(cr){
              cache.get(id) match {
                case None => reply(NotFound())
                case Some(ci) =>
                  ci.result match{
                    case None => reply(PartialContent(ci.progress)) 
                    case Some(Failure(e)) => reply(ServerError(some(e)))
                    case Some(Success(r)) => reply(Ok(r, ci.originalQuery))
                  }
              }
              //reply(ServerError(None))
            }
          case HCancelJob(id, cr) =>
            log_debug("Asked to try cancel job id="+id.name)
            withAuthorization(cr){
              cache.get(id) match {
                case None => reply(NotFound())
                case Some(ci) =>
                  worker ! CancelJob(id)
                  reply(Accepted(id))
              }
              //reply(ServerError(None))
            }
          case ReportJobEnding(id, result) =>
            log_debug("Job "+id.name+" ended")
            cache(id).result = some(result)
            cache(id).progress = 1.0
          case ReportJobProgress(id, progress) =>
            //log_debug("Job "+id.name+" "+(progress*100)+"% done")
            cache(id).progress = progress
          case x => println(x)
        }
      } catch {
        case e:Exception =>
          e.printStackTrace()
          log_error(e.getMessage)
      }
    }
  }
}
