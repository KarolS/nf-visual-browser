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
 * A subpanel displaying captions for a chart.
 * <br>
 * Podpanel wyświetlający legendę do wykresu.
 */
class Caption(captions: Seq[String])(exportClick: =>Unit) extends JPanel{
  val subPanel = new JPanel
  val lb = withLayout(
    subPanel, 
    "5dlu,f:20dlu,5dlu,p:g,5dlu", 
    "5dlu,"
    + captions.map(_=>"f:p").mkString(",5dlu,")
    + ",5dlu"
  )
  0 until (captions.length) zip captions foreach {
    case (row, text) =>
      lb.add(2, 2*row+2, colour(Colours(row)))
      lb.add(4, 2*row+2, text)
  }
  
  withLayout(this,"c:p","p,p,5dlu:g").
  add(1,1,subPanel)/*.
  add(1,2,"5dlu,p","p"){
    _.add(2,1,button("Export image"){
      exportClick
    })
  }*/
  
}
