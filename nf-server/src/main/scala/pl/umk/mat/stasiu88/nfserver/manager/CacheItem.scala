/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.manager
import org.joda.time.{Instant, Duration}
import scala.actors.Futures._
import scala.actors.Future
import pl.umk.mat.stasiu88.nfserver.worker.Result
import scalaz._ 
import Scalaz._

object CacheItem{
  /**
   * Creates a new CacheItem representing a new query.
   * <br>
   * Tworzy nowy CacheItem reprezentujący nowe zapytanie.
   */
  def apply(
      originalQuery: String,
      sender: String,
      timeout: Instant
      ) = {
    new CacheItem(originalQuery, sender, timeout)
  }
  val ONE_HOUR = Duration.standardHours(1)
  def apply(
      originalQuery: String,
      sender: String,
      timeout: Duration
      ):CacheItem = apply(originalQuery, sender, Instant.now().plus(timeout))
  def apply(
      originalQuery: String,
      sender: String
      ):CacheItem = apply(originalQuery, sender, ONE_HOUR)
}

/**
 * Stores data associated with a job.
 * <br>
 * Przechowuje dane związane z zadaniem.
 */
class CacheItem(
    val originalQuery: String,
    val sender:String,
    val timeout: Instant,
    var result: Option[Validation[Throwable,Result]] = None,
    var progress: Double = 0.0
    ) {
  def isFinished = result.isDefined
}