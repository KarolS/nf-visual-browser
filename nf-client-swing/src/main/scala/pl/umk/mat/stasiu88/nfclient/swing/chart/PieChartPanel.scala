package pl.umk.mat.stasiu88.nfclient.swing.chart

import javax.swing._
import java.awt._
import pl.umk.mat.stasiu88.nfclient.chart._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import scala.xml.Node
import scala.xml.NodeSeq

class PieChartPanel(val name:String, dataType: DataType.Value, chart: PieChart) extends ChartPanel {
  val tabs = new JTabbedPane
  
  val pieSubpanel = new PieChartSubPanel(dataType, chart)
  tabs.add("Pie chart", pieSubpanel)
  val tableSubpanel = new Table2ChartPanel(name, dataType, chart.asTable2)
  tabs.add("Table", tableSubpanel)
  
  withOneComponent(this, tabs)
  
  def exportToHtml(dirWithSep: String):NodeSeq = {
    val table = tableSubpanel.exportToHtmlWithColours(dirWithSep)
    val chartFile = dirWithSep + name.toLowerCase.map{ c =>
      if ((c>='0' && c<='9') || (c>='a' && c<='z')) c
      else '_'
    } + ".png"
    exportPng(chartFile, 500,500){pieSubpanel.draw _}
    val image = <p>
      <img src={chartFile}/>
    </p>
    table ++ image
  }
}
class PieChartSubPanel(dataType: DataType.Value, chart: PieChart) extends JPanel{
  withLayout(
    this,
    "f:p:g,100dlu",
    "f:p:g"
  ).
  add(1,1,canvas(draw _)).
  add(2,1,
    new Caption (0 until chart.length map {chart period _})(())
  )
  
  def draw(g: Graphics, w: Int, h: Int){
    val r = (w.min(h)*0.45).toInt
    val sx = w/2-r
    val sy = h/2-r
    var sum = 0.0
    for(i <- 0 until chart.length){
      val p = chart.percentage(i)*360
      g.setColor(Colours(i))
      val newsum = sum + p
      g.fillArc(sx, sy, 2*r, 2*r, sum.toInt, newsum.toInt)
      sum = newsum
    }
  }
}
