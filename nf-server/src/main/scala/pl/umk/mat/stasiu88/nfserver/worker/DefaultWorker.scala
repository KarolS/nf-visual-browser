/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.worker

import scala.actors.Actor
import scala.collection.mutable.HashMap
import pl.umk.mat.stasiu88.nfserver.manager.Manager
import pl.umk.mat.stasiu88.nfserver.messages._
import scalaz._
import Scalaz._
import com.twitter.util.Future
import pl.umk.mat.stasiu88.nfserver.datasource._
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Logging

/**
 * A composable trait providing a default worker
 * <br>
 * Składalna cecha dostarczająca domyślnego wykonawcę.
 */
trait DefaultWorkerComponent extends WorkerComponent {
  this: DataSourceComponent =>
  lazy val worker = new DefaultWorker
}

class DefaultWorker extends Worker with Logging{
  start()
  val jobs = new HashMap[Symbol, com.twitter.util.Future[Unit]]
  
  private[this] var dataSource:DataSource = null
  
  def init(dataSource:DataSource){
    this.dataSource = dataSource
  }
  
  def act(){
    loop {
      try{
        react{
          case StartNewJob(id, query)=> {
            log_debug("Worker starts new job: "+id.name+" "+query)
            val sendResultThere = sender
            val future = Future{
              try{
                val q = Query(query)
                val result = dataSource.getResult(q){ p=>
                  sendResultThere ! ReportJobProgress(id,p)                
                }.clipIfNeeded(q)
                sendResultThere ! ReportJobEnding(id, success(result))
              }catch{
                case e => 
                  sendResultThere ! ReportJobEnding(id, failure(e)) 
              }
            }
            jobs += id -> future
            
          }
          case CancelJob(id) => {
            log_debug("Worker cancels job: "+id)
            jobs(id).cancel()
          }
        }
      } catch {
        case e:Exception =>
          e.printStackTrace()
          log_error(e.getMessage)
      }
    }
  }
}