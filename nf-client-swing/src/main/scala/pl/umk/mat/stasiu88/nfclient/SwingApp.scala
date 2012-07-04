/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient

import pl.umk.mat.stasiu88.nfclient.swing.MainWindow
import pl.umk.mat.stasiu88.nfclient.agent.DefaultAgentComponent
import pl.umk.mat.stasiu88.nfclient.swing.SwingUIComponent
import pl.umk.mat.stasiu88.nfclient.timer.TimerComponent
import pl.umk.mat.stasiu88.nfclient.logging.ConsoleLogging

/**
 * Main entrypoint for the Swing client.
 * <br>
 * Główny punkt wejścia dla klienta Swing.
 */
object SwingApp extends ConsoleLogging  { //TODO: do it smarter
  def main(args: Array[String]){
    try{
      val cake = (
          new  DefaultAgentComponent
          with SwingUIComponent
          with TimerComponent
      )
      cake.startTimer()
      cake.startUI()
    } catch {
      case e:Exception =>
        log_fatal(e.toString)
        System exit 1
    }
  }
}