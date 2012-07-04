/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.ui
import scala.actors.Actor
import pl.umk.mat.stasiu88.nfclient.agent.{Agent, AgentComponent}

/**
 * Composable trait providing UI.
 * <br>
 * Składalna cech dostarczająca UI.
 */
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