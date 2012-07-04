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
 * A panel displaying a cumulative chart and the data table.
 * <br>
 * Panel wyświetlający wykres skumulowany i tabelę z danymi.
 */
class CumulativeChartPanel(val name:String, dataType: DataType.Value, chart: CumulativeChart) extends ChartPanel{
  val tabs = new JTabbedPane
  
  val graphSubpanel = new CumulativeChartGraphSubPanel(dataType, chart)
  tabs.add("Graph", graphSubpanel)
  val tableSubpanel = new CumulativeChartTableSubPanel(dataType, chart)
  tabs.add("Table", tableSubpanel)
  
  withOneComponent(this, tabs)
  def exportToHtml(dirWithSep: String) = {
    <p>
  <h1>{name}</h1>
  <table border="1">
  <tr>
  <th></th>{
    (0 until chart.categoryCount) map { c=>
      <th>{chart.category(c)}</th>
    }
  }
  </tr>
  <tr>
  <td>Period:</td>{
    (0 until chart.categoryCount) map { c=>
      <td style={"background: #"+Colours.inHex(c)}>&nbsp;</td>
    }
  }
  </tr>
  {
    val multiplier = DataUnit.size(tableSubpanel.model.unit)
    val suffix = DataUnit.symbol(tableSubpanel.model.unit)
    (0 until chart.periodCount)map{ p=>
      <tr>
      
      <td>{chart.period(p)}</td>{
        (0 until chart.categoryCount)map{c =>
          <td>{
            if(multiplier == 1L) {
              chart.get(c,p).toString+suffix
            } else {
              "%1.2f".format(chart.get(c,p).toDouble/multiplier)+suffix
            }
          }</td>
        }
      }</tr>
    }
  }
  </table>{
    val chartFile = dirWithSep + name.toLowerCase.map{ c =>
      if ((c>='0' && c<='9') || (c>='a' && c<='z')) c
      else '_'
    } + ".png"
    exportPng(chartFile, 500,500){graphSubpanel.draw _}
    <img src={chartFile}/>
  }</p>

  }
}



class CumulativeChartTableSubPanel(val dataType: DataType.Value, val chart: CumulativeChart) extends JPanel {
  val model = new CumulativeChartTableModel(chart)
  model.setUnit(DataType.units(dataType).head)
  
  val unitList = new JComboBox
  unitList.setEditable(false)
  unitList.setModel(new DefaultComboBoxModel(Array[AnyRef](
    DataType.units(dataType).map{ u =>
      UnitListItem(DataUnit.name(u),u)
    } : _*
  )))
  unitList.setSelectedIndex(0)
  unitList.withAction{
    model.setUnit(unitList.getSelectedItem.asInstanceOf[UnitListItem].unit)
    model.fireTableDataChanged()
  }
  withLayout(this, "p,5dlu,f:p:g", "p,5dlu,f:p:g").
  add(1,3, 3,1, scrollableTable(model)).
  add(1,1, unitList)
}

class CumulativeChartTableModel(val chart: CumulativeChart) extends AbstractTableModel {
  var unit: DataUnit.Value = DataUnit.Packets
  def multiplier = DataUnit.size(unit)
  def suffix = DataUnit.symbol(unit)
  
  def setUnit(unit: DataUnit.Value) {
    this.unit = unit
  }
  def getRowCount = chart.periodCount
  def getColumnCount = 1 + chart.categoryCount
  override def getColumnName(i:Int) = i match {
    case 0 => ""
    case a => chart.category(a-1)
  } 
  def getValueAt(j:Int, i:Int) = try{
    i match {
      case 0 => chart.period(j)
      case _ => if(multiplier == 1L) {
        chart.get(i-1, j).toString+suffix
      } else {
        "%1.2f".format(chart.get(i-1, j).toDouble/multiplier).toString+suffix
      }
    }
  } catch {
    case _ => 
      "Index out of bounds: "+(i,j)
  }
  override def isCellEditable(x:Int, y: Int) = false
  override def setValueAt(o: AnyRef, x: Int, y: Int) = ()
}