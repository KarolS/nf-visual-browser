/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.agent
import scala.actors.Actor
import pl.umk.mat.stasiu88.nfclient.ui.{UI, UIComponent}

/**
 * Composable trait providing an agent.
 * <br>
 * Składalna cecha dostarczająca agenta.
 */
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