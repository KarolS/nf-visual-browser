package pl.umk.mat.stasiu88.nfclient.ui
import scala.actors.Actor
import pl.umk.mat.stasiu88.nfclient.agent.{Agent, AgentComponent}

trait UIComponent {
  this: AgentComponent =>
 
  def ui:UI
  def startUI(): Unit

  ui.init(agent)
  ui.start()
}

trait UI extends Actor{
  def init(agent: Agent)
}