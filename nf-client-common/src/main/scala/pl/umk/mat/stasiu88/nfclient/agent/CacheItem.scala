package pl.umk.mat.stasiu88.nfclient.agent

import pl.umk.mat.stasiu88.nfclient.chart.Statistic
import scalaz._
import Scalaz._

sealed trait CacheItem{
  def isRefreshable: Boolean
}

case class SuccessfulItem(data: List[Statistic]) extends CacheItem {
  def isRefreshable = false
}
case object FreshItem extends CacheItem {
  def isRefreshable = true
}
case class FailedItem(reason: String) extends CacheItem {
  def isRefreshable = false
}
case class ItemInProgress(progress: Double) extends CacheItem {
  def isRefreshable = true
}
case object NonExistentItem extends CacheItem {
  def isRefreshable = false
}
case object CancelledItem extends CacheItem {
  def isRefreshable = false
}