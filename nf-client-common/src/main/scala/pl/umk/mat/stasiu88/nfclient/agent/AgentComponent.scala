package pl.umk.mat.stasiu88.nfclient.agent
import scala.actors.Actor
import pl.umk.mat.stasiu88.nfclient.ui.{UI, UIComponent}
trait AgentComponent {
  this: UIComponent =>
  def agent: Agent
  // required so it works:
  agent.init(this.ui)
  agent.start()
}
trait Agent extends Actor{
  def init(ui: UI)
  def cacheItem(id: Symbol): CacheItem
}