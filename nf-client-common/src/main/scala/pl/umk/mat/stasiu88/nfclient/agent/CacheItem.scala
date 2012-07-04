/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.agent

import pl.umk.mat.stasiu88.nfclient.chart.Statistic
import scalaz._
import Scalaz._

/**
 * An item of agent's cache, containing request status and/or results.
 * <br>
 * Element cache agenta, zawierający status i/lub wyniki zapytania. 
 */
sealed trait CacheItem{
  /**
   * Should the agent try to refresh this item by querying the server?
   * <br>
   * Czy agent powinien odświeżyć ten element, przez odpytanie serwera? 
   */
  def isRefreshable: Boolean
}

/**
 * Item with results of successful query.
 * <br>
 * Element z wynikami udanego zapytania.
 */
case class SuccessfulItem(data: List[Statistic]) extends CacheItem {
  def isRefreshable = false
}
/**
 * Item representing a new query.
 * <br>
 * Element reprezentujący nowe zapytanie.
 */
case object FreshItem extends CacheItem {
  def isRefreshable = true
}
/**
 * Item representing a failed query.
 * <br>
 * Element reprezentujący nieudane zapytanie.
 */
case class FailedItem(reason: String) extends CacheItem {
  def isRefreshable = false
}
/**
 * Item representing a query in progress.
 * <br>
 * Element reprezentujący trwające zapytanie.
 */
case class ItemInProgress(progress: Double) extends CacheItem {
  def isRefreshable = true
}
/**
 * Item representing a non-existent query.
 * <br>
 * Element reprezentujący nieistniejące zapytanie.
 */
case object NonExistentItem extends CacheItem {
  def isRefreshable = false
}
/**
 * Item representing a cancelled query.
 * <br>
 * Element reprezentujący anulowane zapytanie.
 */
case object CancelledItem extends CacheItem {
  def isRefreshable = false
}