package pl.umk.mat.stasiu88.nfclient.ui

import pl.umk.mat.stasiu88.nfclient.agent.AgentComponent
import pl.umk.mat.stasiu88.nfclient.messages._
import pl.umk.mat.stasiu88.nfclient.agent.Agent
import pl.umk.mat.stasiu88.nfclient.logging.ConsoleLogging

trait ConsoleUIComponent extends UIComponent {
  this: AgentComponent =>
  lazy val ui = new ConsoleUI
  def startUI(){}
}
class ConsoleUI extends UI with ConsoleLogging {
  private[this] var agent: Agent = null
  def init(agent: Agent){
    this.agent = agent    
    log_debug("Got an agent")
  }
  def act() = loop {
    log_trace("UI waits for a message")
    react {
      case JobAccepted(id) =>
        log_info("Job accepted with id="+id.name)
      case JobCancelled(id) =>
        log_info("Job "+id.name+" cancelled")
      case JobInterrupted(id) =>
        log_info("Job "+id.name+" interrupted")
      case JobDone(id) =>
        log_info("Job "+id.name+" done")
      case JobInProgress(id) =>
        log_info("Job "+id.name+" still in progress")
      case JobUnknown(id) =>
        log_info("Job "+id.name+" did something weird")
      case ServerBusy => 
        log_error("Server busy")
      case ServerError(err) =>
        log_error("Server error: "+err)
      case ServerNotResponding => 
        log_error("Server not responding")
      case InvalidCredentials => 
        log_error("Invalid credentials")
      case _ =>
        log_warn("Unexpected message type")
    }
  }
}