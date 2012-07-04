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

/**
 * A panel displaying a cumulative chart.
 * <br>
 * Panel wyświetlający wykres skulumowany.
 */
class CumulativeChartGraphSubPanel(dataType: DataType.Value, chart: CumulativeChart) extends JPanel{
  withLayout(
    this,
    "f:p:g,100dlu",
    "f:p:g"
  ).
  add(1,1,canvas(draw _)).
  add(2,1,
    new Caption (
      (0 until chart.categoryCount) map {
        chart.category(_)
      }
    )({})
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
    val width = (w-sx-MR).toDouble/chart.periodCount
    g.setColor(Color.BLACK)
    g.drawLine(ML,MT, ML,h-MB)
    g.drawLine(ML,h-MB, w-MR,h-MB)
    
    for(i <- 0 until chart.periodCount){
      val x = sx+(width*i).toInt
      val x1 = sx+(width*(i+1)).toInt
      val colw = 1 max (x1-x)
      for (cat <- 0 until chart.categoryCount){
        g.setColor(Colours(cat))
        var sum = 0L
        for (cat2 <- cat until chart.categoryCount) {
          sum += chart.get(cat2,i)
        }
        val y = (sy-scale*sum).toInt
        g.fillRect(x, y, colw, sy-y)
      }
    }
    g.setFont(FONT)
    g.setColor(Color.BLACK)
    
    if(chart.periodCount==0) return
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
    for(i <- 0 until chart.periodCount){
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
