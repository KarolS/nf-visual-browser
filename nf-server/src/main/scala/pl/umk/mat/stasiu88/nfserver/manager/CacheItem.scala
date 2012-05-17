package pl.umk.mat.stasiu88.nfserver.manager
import org.joda.time.{Instant, Duration}
import scala.actors.Futures._
import scala.actors.Future
import pl.umk.mat.stasiu88.nfserver.worker.Result
import scalaz._ 
import Scalaz._

object CacheItem{
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
class CacheItem(
    val originalQuery: String,
    val sender:String,
    val timeout: Instant,
    var result: Option[Validation[Throwable,Result]] = None,
    var progress: Double = 0.0
    ) {
  def isFinished = result.isDefined
  def tryToCancel = () //TODO
}