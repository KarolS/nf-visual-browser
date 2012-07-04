/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.timer

import pl.umk.mat.stasiu88.nfclient.agent.AgentComponent
import pl.umk.mat.stasiu88.nfclient.Utils._
import pl.umk.mat.stasiu88.nfclient.messages.RefreshAllQueries

/**
 * Composable trait providing a timer. The timer sends <code>RefreshAllQueries</code> to the agent every 5 seconds.
 * <br>
 * Składalna cecha dostarczająca zegara. Zegar wysyła <code>RefreshAllQueries</code> do agenta co pięć sekund.
 */
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
  
  /**
   * Starts the timer.
   * <br>
   * Uruchamia zegar.
   */
  def startTimer() ={
    th.start()
  }
  
}