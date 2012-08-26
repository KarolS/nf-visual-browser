/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing

import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import pl.umk.mat.stasiu88.nfclient.ui._
import pl.umk.mat.stasiu88.nfclient.agent._
import scala.collection.mutable
import javax.swing._

/**
 * Composable trait providing a Swing UI.
 * <br>
 * Składalna cecha dostarczająca UI w Swing.
 */
trait SwingUIComponent extends UIComponent {
  this: AgentComponent =>
  lazy val ui = new SwingUI
  def startUI() {
    MainWindow.setVisible(true)
  }
}
class SwingUI extends BaseUI{
  override def init(agent: Agent) {
    super.init(agent)
    MainWindow.agent = agent
    MainWindow.ui = this
  }
  val tabMap = mutable.Map[Symbol, OldQueryTab]()
  def reactToError(id:Symbol, error: String){
    errorMessage("For query id="+id.name+"\n"+error)
  }
  def reactToTimeout(id:Symbol){
    errorMessage("Timeout for query id="+id.name)
  }
  def reactToError(error: String){
    errorMessage(error)
  }
  def reactToInvalidCredentials(){
    errorMessage("Invalid username/password")
    MainWindow.openCredentialsWindow(false)
  }
  def reactToServerBusy(){
    errorMessage("The server is busy")
  }
  def reactToServerNotResponding(){
    errorMessage("The server does not respond")
  }
  def reactToServerStatusOk(){
    infoMessage("Server status: OK")
  }
  def refreshUI(changedId: Symbol) = synchronized {
    queries.find(_.id==changedId) match {
      case Some(q) =>
        if(tabMap.contains(changedId)){
          //Updated
          invokeAndWait{
            tabMap(changedId).refresh(q.query)
            tabMap(changedId).repaint()
          }
        }
        else{
          //Created
          tabMap(changedId) = new OldQueryTab(changedId)
          invokeAndWait {
            MainWindow.tabs.add(tabMap(changedId), q.description)
          }
        }
        invokeAndWait{
          tabMap(changedId).refresh(q.query)
          tabMap(changedId).repaint()
        }
      case None =>
        //Deleted
        invokeAndWait {
          MainWindow.tabs.remove(tabMap(changedId))
        }
    }
  }
}