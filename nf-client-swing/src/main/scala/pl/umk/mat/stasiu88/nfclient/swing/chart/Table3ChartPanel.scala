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
 * A panel displaying a data table with three columns.
 * <br>
 * Panel wyświetlający tabelę z danymi o trzech kolumnach.
 */
class Table3ChartPanel(val name:String, dataType: DataType.Value, chart: Table3) extends ChartPanel{
  val model = new Table3TableModel(chart)
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
    unit = unitList.getSelectedItem.asInstanceOf[UnitListItem].unit
    model.setUnit(unit)
    model.fireTableDataChanged()
  }
  var unit = DataType.units(dataType).head
  
  withLayout(this, "p,5dlu,f:p:g", "p,5dlu,f:p:g").
  add(1,3, 3,1, scrollableTable(model)).
  add(1,1, unitList)
  
  def exportToHtml(dirWithSep: String): Node = <p>
  <h1>{name}</h1>
  <table border="1">
  <tr><th></th><th>Value</th></tr>
  {
    val multiplier = DataUnit.size(unit)
    val suffix = DataUnit.symbol(unit)
    var lastPeriod = "this is not the last period sdkfjldsflkslfsfsd"
    (0 until chart.length)map{ i=>
      <tr>
      <td>{
        val thisPeriod = chart.period(i)
        if(thisPeriod == lastPeriod) ""
        else{
          lastPeriod = thisPeriod
          thisPeriod
        }
      }</td>
      <td>{chart.index(i)}</td>
      <td>{
        if(multiplier == 1L) {
          chart.get(i).toString+suffix
        } else {
          "%1.2f".format(chart.get(i).toDouble/multiplier)+suffix
        }
      }</td>
      </tr>
    }
  }
  </table>
  </p>
}

class Table3TableModel(val chart: Table3) extends AbstractTableModel {
  var unit: DataUnit.Value = DataUnit.Packets
  def multiplier = DataUnit.size(unit)
  def suffix = DataUnit.symbol(unit)
  
  def setUnit(unit: DataUnit.Value) {
    this.unit = unit
  }
  def getRowCount = chart.length
  def getColumnCount = 3
  override def getColumnName(i:Int) = i match {
    case 0 => "Period"
    case 1 => "Highest value at"
    case 2 => "Value"
  } 
  def getValueAt(j:Int, i:Int) = try{
    i match {
      case 0 => chart.period(j)
      case 1 => chart.index(j)
      case 2 => if(multiplier == 1L) {
        chart.get(j).toString+suffix
      } else {
        "%1.2f".format(chart.get(j).toDouble/multiplier).toString+suffix
      }
    }
  } catch {
    case _ => 
      "Index out of bounds: "+(i,j)
  }
  override def isCellEditable(x:Int, y: Int) = false
  override def setValueAt(o: AnyRef, x: Int, y: Int) = ()
}