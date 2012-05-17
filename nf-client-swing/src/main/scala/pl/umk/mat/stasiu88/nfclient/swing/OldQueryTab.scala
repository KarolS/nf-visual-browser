package pl.umk.mat.stasiu88.nfclient.swing

import javax.swing._
import com.jgoodies.forms.layout.FormLayout
import com.jgoodies.forms.layout.CellConstraints
import pl.umk.mat.stasiu88.nfclient.ui._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import pl.umk.mat.stasiu88.nfclient.messages.CancelQuery
import pl.umk.mat.stasiu88.nfclient.messages.CancelQuery
import java.io.File
import pl.umk.mat.stasiu88.nfclient.messages.RefreshQuery

class OldQueryTab(id:Symbol) extends JPanel {

  private[this]var theComponent: JComponent = null
  
  def beforeClosing() {
    theComponent match{
      case null =>
      case _:QueryInProgressPanel =>
        MainWindow.agent ! CancelQuery(id)
      case _:QuerySentPanel =>
        MainWindow.agent ! CancelQuery(id)
      case _ => 
    }
  }
  
  def exportToHtml() {
    theComponent match {
      case pwc:PanelWithCharts =>
        pwc.exportToHtml()
      case _ =>
    }
  }
  
  def setOnlyComponent(c: JComponent) {
    removeAll()
    theComponent = c
    withOneComponent(this, c)
    updateUI()
  }
  def refresh(q: QueryForUI) = {
    q match {
      case FreshQueryForUI =>
        setOnlyComponent(new QuerySentPanel(id))
      case CancelledQueryForUI =>
        setOnlyComponent(new QueryCancelledPanel)
      case QueryInProgressForUI(progress) =>
        setOnlyComponent(new QueryInProgressPanel(id, progress))
      case FinishedQueryForUI(c) =>
        setOnlyComponent(new PanelWithCharts(c))
      case FailedQueryForUI(f) =>
        setOnlyComponent(new FailedQueryPanel(id,f))
      case _ => 
        println("OldQueryTab: [ERROR] Unknown message type") //TODO: temporary
    }
    println(q)//TODO
  }
}

class QuerySentPanel(id: Symbol) extends JPanel{
  withLayout(this,"c:p:g","p:g,p,5dlu,p,p:g").
  add(1,2,"Request successfully sent").
  add(1,4,button("Cancel"){
    confirm("Do you really want ot cancel the query"){
      MainWindow.agent ! CancelQuery(id)
    }
  })
}
class QueryCancelledPanel extends JPanel{
  withLayout(this,"c:p:g","c:p:g").add(1,1,"Request cancelled")
}
class QueryInProgressPanel(id: Symbol, progress: Double) extends JPanel{
  withLayout(this,"c:p:g","p:g,p,5dlu,p,5dlu,p,p:g").
  add(1,2,"Request in progress").
  add(1,4,new JProgressBar{
    setMaximum(1000)
    setMinimum(0)
    setValue((1000*progress).toInt)
  }).
  add(1,6,button("Cancel"){
    confirm("Do you really want ot cancel the query"){
      MainWindow.agent ! CancelQuery(id)
    }
  })
}
class FailedQueryPanel(id: Symbol, reason: String) extends JPanel {
  withLayout(this,"c:p:g","0dlu:g,p,5dlu,p,5dlu,p,0dlu:g").
  add(1,2, "Request failed").
  add(1,4, reason.toString).
  add(1,6, button("Retry"){
    MainWindow.agent ! RefreshQuery(id)
  })
} 
