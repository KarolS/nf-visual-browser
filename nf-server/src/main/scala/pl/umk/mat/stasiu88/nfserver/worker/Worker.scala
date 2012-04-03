package pl.umk.mat.stasiu88.nfserver.worker

import scala.actors.Actor
import scala.collection.mutable.HashMap
import pl.umk.mat.stasiu88.nfserver.messages._
import scalaz._
import Scalaz._
import com.twitter.util.Future
import pl.umk.mat.stasiu88.nfserver.datasource._
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Logging

object Worker extends Actor with Logging{
  start()
  val jobs = new HashMap[Symbol, com.twitter.util.Future[Unit]]
  
  def dataSource:DataSource = new DirectoryDataSource("/media/DOKUMENTY/NF") //TODO

  def act(){
    loop {
      react{
        case StartNewJob(id, query)=> {
          log_debug("Worker starts new job: "+id+" "+query)
          val sendResultThere = sender
          val future = Future{
            try{
              val result = dataSource.getResult(Query(query))
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
    }
  }
}