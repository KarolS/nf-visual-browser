/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing.chart

import javax.swing._
import java.awt._
import pl.umk.mat.stasiu88.nfclient.chart._
import pl.umk.mat.stasiu88.nfclient.SwingUtils._
import javax.swing.table.AbstractTableModel
import scala.xml.Node

/**
 * A panel displaying a bar chart and the data table.
 * <br>
 * Panel wyświetlający wykres słupkowy i tabelę z danymi.
 */
class GraphableTable2ChartPanel(val name:String, colour: Int, dataType: DataType.Value, chart: GraphableTable2) extends ChartPanel{
  val tabs = new JTabbedPane
  
  val graphSubpanel = new GraphableTable2ChartSubPanel(colour, dataType, chart)
  tabs.add("Graph", graphSubpanel)
  val tableSubpanel = new Table2ChartPanel(name, dataType, chart.asTable2)
  tabs.add("Table", new Table2ChartPanel(name, dataType, chart.asTable2))
  
  withOneComponent(this, tabs)
  
  def exportToHtml(dirWithSep: String) = {
    val table = tableSubpanel.exportToHtml(dirWithSep)
    val chartFile = dirWithSep + name.toLowerCase.map{ c =>
      if ((c>='0' && c<='9') || (c>='a' && c<='z')) c
      else '_'
    } + ".png"
    exportPng(chartFile, chart.length+200 max 700 ,500){graphSubpanel.draw _}
    val image = <p>
      <img src={chartFile}/>
    </p>
    table ++ image
  }
}

/**
 * A panel displaying a bar chart.
 * <br>
 * Panel wyświetlający wykres słupkowy.
 */
class GraphableTable2ChartSubPanel(colour: Int, dataType: DataType.Value, chart: GraphableTable2) extends JPanel{
  withLayout(
    this,
    "f:p:g,100dlu",
    "f:p:g"
  ).
  add(1,1,canvas(draw _)).
  add(2,1,
    new Caption (Seq(DataType toString dataType))({})
  )

  val FONT = new Font("Arial",0,10)
  
  def draw(g: Graphics, w: Int, h:Int){
    val ML = 60
    val MR = 10
    val MT = 10
    val MB = 50
    val sx = ML
    val sy = h - MB
    val max = chart.max
    val scale = (sy-MT).toDouble/max
    val width = (w-sx-MR).toDouble/chart.length
    g.setColor(Color.BLACK)
    g.drawLine(ML,MT, ML,h-MB)
    g.drawLine(ML,h-MB, w-MR,h-MB)
    g.setColor(Colours(colour))
    for(i <- 0 until chart.length){
      val x = sx+(width*i).toInt
      val x1 = sx+(width*(i+1)).toInt
      val colw = 1 max (x1-x)
      val y = (sy-scale*chart.get(i)).toInt
      g.fillRect(x, y, colw, sy-y)
    }
    g.setFont(FONT)
    g.setColor(Color.BLACK)
    
    if(chart.length==0) return
    val periodType = PeriodType.guess(chart.period(0))
    val perUnit = PeriodType.per(periodType)
    
    val (step, unitname) = DataType.pickScaleStep(dataType, scale, h/10)
    var level = 1L
    while((sy-scale*level*step) > MT){
      val y = (sy-scale*level*step).toInt
      g.drawLine(ML-2,y,ML,y)
      val string = level.toString+unitname+perUnit
      val tw = g.getFontMetrics.getStringBounds(string, g).getWidth.toInt
      g.drawString(string, ML-tw-3, y)
      level+=1
    }
    
    val dx = if(PeriodType.alignToTheLeft(chart.period(0))) 0.0 else 0.5
    var noofDrawnPeriods = 0
    for(i <- 0 until chart.length){
      val period = chart.period(i)
      if(PeriodType.isImportant(periodType, period, width)){
        val lx = sx+(width*(i+dx)).toInt
        val tx = sx+(width*(i+dx)-g.getFontMetrics.getStringBounds(period, g).getWidth/2.0).toInt
        val y = h - MB + 20 + (noofDrawnPeriods%2)*20
        g.drawString(period, tx, y)
        g.drawLine(lx,h-MB-5,lx,h-MB+5+10*(noofDrawnPeriods%2))
        noofDrawnPeriods+=1
      }
    }
  }
}
