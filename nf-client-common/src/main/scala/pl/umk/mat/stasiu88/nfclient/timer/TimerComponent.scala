package pl.umk.mat.stasiu88.nfclient.timer

import pl.umk.mat.stasiu88.nfclient.agent.AgentComponent
import pl.umk.mat.stasiu88.nfclient.Utils._
import pl.umk.mat.stasiu88.nfclient.messages.RefreshAllQueries

trait TimerComponent {
  this: AgentComponent =>
    
  private[this]val th = thread {
    try{
      while(true){
        Thread sleep 5000
        agent ! RefreshAllQueries
      }
    }
    catch{
      case _ => ()
    }
  }
  
  def startTimer() ={
    th.start()
  }
  
}