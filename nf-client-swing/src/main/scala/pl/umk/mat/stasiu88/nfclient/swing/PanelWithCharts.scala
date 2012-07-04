/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing

import javax.swing._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import pl.umk.mat.stasiu88.nfclient.chart._
import pl.umk.mat.stasiu88.nfclient.swing.chart._
import java.io.File
import scala.xml.XML
import java.io.FileWriter
import scala.xml.dtd.DocType

/**
 * A panel syiplaying charts and tables.
 * <br>
 * Panel wyświetlający wykresy i tabele.
 */
class PanelWithCharts(val statCharts: List[StatChart]) extends JPanel {
  val tabs = new JTabbedPane
  val subpanels:List[ChartPanel] = statCharts.flatMap{ sc =>
    val ccs: List[(Int,String,Chart)] = sc.categoryCharts.zipWithIndex.map{ case (cc,idx) =>
      (idx,sc.statistic+" - "+cc.category, cc.chart)
    }
    sc.crossCategoryChart match { 
      case Some(ccc) =>
        (0,sc.statistic+" - by category", ccc) :: ccs
      case None =>
        ccs
    }
  }.flatMap{ case(idx,name,chart) =>
    //println(name, chart)
    val p = PanelWithCharts(name, chart, idx)
    p.foreach{tabs.add(name, _)}
    p
  }
  
  withLayout(this,"f:p:g","f:p:g").add(1,1,tabs)
  
  def exportToHtml() = {
    chooseFile(this) foreach { indexHtml =>
      //val pathSep = File.pathSeparator
      //val dirWithSep = file.getAbsolutePath.stripSuffix(pathSep)+pathSep
      //val indexHtml = dirWithSep+"index.html"
      val index = <html>
    <head><title>Report</title></head>
    <body>{subpanels.map{_.exportToHtml(indexHtml+".")}}</body>
    </html>
      val writer = new FileWriter(indexHtml)
      try{
        XML.write(
          writer,
          index,
          "UTF-8",
          false, 
          DocType( "html", xml.dtd.SystemID( "about:legacy-compat" ), Nil )
        )
        JOptionPane.showMessageDialog(this, "HTML file saved.")
      } catch {
        case e:Exception =>
          e.printStackTrace()
          errorMessage(e.getMessage)
          throw e
      } finally {
        writer.close()
      }
    }
  }
}

object PanelWithCharts {
  def apply(statisticName:String, c:Chart, colourIdx: Int): Option[ChartPanel] ={
    val st = statisticName(0) match {
      case 'b' => DataType.Bytes
      case 'B' => DataType.Bytes
      case 'f' => DataType.Flows
      case 'F' => DataType.Flows
      case 'p' => DataType.Packets
      case 'P' => DataType.Packets
      case 'd' => DataType.Duration
      case 'D' => DataType.Duration
      case  _  => DataType.Packets //TODO: dumb safeguard
    }
    c match {
      case c:PieChart => Some(new PieChartPanel(statisticName,st,c))
      case c:Table2 => Some(new Table2ChartPanel(statisticName,st,c))
      case c:Table3 => Some(new Table3ChartPanel(statisticName,st,c))
      case c:GraphableTable2 => Some(new GraphableTable2ChartPanel(statisticName,colourIdx,st,c))
      case c:CumulativeChart => Some(new CumulativeChartPanel(statisticName,st,c))
      case _ => None
    }
  }
}
