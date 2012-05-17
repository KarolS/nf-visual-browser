package pl.umk.mat.stasiu88.nfclient.swing

import javax.swing._
import javax.swing.JFrame._
import com.jgoodies.forms.layout.FormLayout
import com.jgoodies.forms.layout.CellConstraints
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import pl.umk.mat.stasiu88.nfclient.agent.Agent
import pl.umk.mat.stasiu88.nfclient.swing.dialogs.UsernamePasswordServerDialog
import pl.umk.mat.stasiu88.nfclient.messages.SetServer
import pl.umk.mat.stasiu88.nfclient.messages.SetCredentials
import pl.umk.mat.stasiu88.nfclient.swing.dialogs.ManageSubnetsDialog
import pl.umk.mat.stasiu88.nfclient.swing.dialogs.CustomQueryDialog
import pl.umk.mat.stasiu88.nfclient.messages.NewQuery

object MainWindow extends JFrame {
  var agent: Agent = null
  
  setTitle("NfClient")
  
  val credentialsWindow = new UsernamePasswordServerDialog(this)
  val subnetworksWindow = new ManageSubnetsDialog(this)
  val customqueryWindow = new CustomQueryDialog(this)
  
  setSize(990,700)
  def openCredentialsWindow(closeIfFailed: Boolean){
    invokeLater{
      if( !
        credentialsWindow.doIfChanged {
          case (u,p,s) =>
            agent ! SetServer(s)
            agent ! SetCredentials(u,p)
        }
      ) if(closeIfFailed) setVisible(false)
    }
  }
  override def setVisible(b:Boolean) { 
    super.setVisible(b)
    if(b){
      openCredentialsWindow(true)
    }
  }
  
  val tabs = new JTabbedPane()
  setDefaultCloseOperation(EXIT_ON_CLOSE)
  withLayout(this, "f:p:g", "f:p:g").add(1,1,tabs)
  tabs.addTab("New query", new NewQueryTab)
  
  withMenu(this).
  menu("Client"){
    _.
    item("Custom query..."){
      customqueryWindow.get() foreach { q =>
        agent ! NewQuery(q)
      }
    }.
    item("Change username/password/server..."){
      openCredentialsWindow(false)
    }.
    item("Manage custom subnetworks..."){
      subnetworksWindow.get()
    }.
    separator.
    item("Exit"){
      confirm ("Do you really want to quit?"){
        (0 until tabs.getTabCount)foreach{ tabs getComponentAt _ match {
            case old: OldQueryTab => old.beforeClosing()
            case _ => 
          }
        }
        System exit 0
      }
    }
  }.
  menu("Tab"){
    _.
    item("Export to HTML..."){
      tabs.getSelectedComponent match{
        case old:OldQueryTab =>
          old.exportToHtml()
        case _ =>
      }
    }.
    item("Close"){
      tabs.getSelectedComponent match{
        case old:OldQueryTab =>
          confirm("Do you really want to close this tab?"){
            old.beforeClosing()
            tabs.remove(old)
          }
        case _ =>
      }
    }
  }
  
}