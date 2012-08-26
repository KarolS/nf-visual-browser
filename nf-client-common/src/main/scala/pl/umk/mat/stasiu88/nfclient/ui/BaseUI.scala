/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.ui

import pl.umk.mat.stasiu88.nfclient.messages._
import pl.umk.mat.stasiu88.nfclient.chart._
import pl.umk.mat.stasiu88.nfclient.agent._
import pl.umk.mat.stasiu88.nfclient.Utils._
import org.joda.time._

/**
 * Partial UI implemetation.
 * <br>
 * Częściowa implementacja UI
 */
trait BaseUI extends UI {
  var agent: Agent = agent
  def init(agent: Agent) {
    this.agent = agent
  }
   
  var queries = List[TaggedQueryForUI]()
  
  protected def processResult(id: Symbol, item: CacheItem) {
    if(queries.exists(_.id==id)){
      queries = QueryForUI fromCacheItem item match{
        case None => queries.filterNot(_.id==id)
        case Some(i) => queries.map{ x=>
          if (x.id==id) x.copy(query=i)
          else x
        }
      }
    } else {
      val date = new DateTime().toString("HH:mm:ss")
      QueryForUI.fromCacheItem(item) foreach {q=> 
        queries = TaggedQueryForUI(
            id,
            date,
            q
        ) :: queries
      }
    }
    refreshUI(id)
    item match{
      case FailedItem(f) => reactToError(id,f)
      case _ => ()
    }
  }
  
  def refreshUI(changedId: Symbol):Unit
  def reactToError(id:Symbol, error: String):Unit
  def reactToTimeout(id:Symbol):Unit
  def reactToError(error: String):Unit
  def reactToInvalidCredentials():Unit
  def reactToServerBusy():Unit
  def reactToServerNotResponding():Unit
  def reactToServerStatusOk():Unit
  
  def act = loop{
    react{
      case JobAccepted(id) =>
        processResult(id, agent.cacheItem(id))
      case JobDone(id) =>
        processResult(id, agent.cacheItem(id))
      case JobCancelled(id) =>
        processResult(id, agent.cacheItem(id))
      case JobInterrupted(id) =>
        processResult(id, agent.cacheItem(id))
      case JobInProgress(id) =>
        processResult(id, agent.cacheItem(id))
      case JobUnknown(id) =>
        processResult(id, agent.cacheItem(id))
      case JobError(id,err) =>
        reactToError(id,err)
      case ServerError(err) =>
        reactToError(err)
      case JobTimeout(id) =>
        reactToTimeout(id)
      case InvalidCredentials =>
        reactToInvalidCredentials()
      case ServerBusy =>
        reactToServerBusy()
      case ServerNotResponding =>
        reactToServerNotResponding()
      case ServerStatusOk =>
        reactToServerStatusOk()
      case ForgetQuery(id) =>
        queries = queries.filterNot(_.id == id)
    }
  }
}

case class TaggedQueryForUI(id: Symbol, description: String, query: QueryForUI)

object QueryForUI {
  def fromCacheItem(item: CacheItem): Option[QueryForUI] = item match {
    case ItemInProgress(p) => Some(QueryInProgressForUI(p))
    case FailedItem(f) => Some(FailedQueryForUI(f))
    case SuccessfulItem(data) =>
      val charts = Chart convertStatistics data
      //println(charts) //TODO
      Some(FinishedQueryForUI(charts))
    case CancelledItem => Some(CancelledQueryForUI)
    case FreshItem => Some(FreshQueryForUI)
    case NonExistentItem => None
  }
}

/**
 * A query status and/or results.
 * <br>
 * Status i/lub wynik zapytania.
 */
sealed trait QueryForUI
case object FreshQueryForUI extends QueryForUI
case class QueryInProgressForUI(progress: Double) extends QueryForUI
case class FailedQueryForUI(reason: String) extends QueryForUI
case class FinishedQueryForUI(charts: List[StatChart]) extends QueryForUI
case object CancelledQueryForUI extends QueryForUI