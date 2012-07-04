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
 * A panel displaying a data table with two columns.
 * <br>
 * Panel wyświetlający tabelę z danymi o dwu kolumnach.
 */
class Table2ChartPanel(val name:String, val dataType: DataType.Value, val chart: Table2) extends ChartPanel {
  val model = new Table2TableModel(chart)
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
  
  def exportToHtml(dirWithSep: String): Node = exportToHtml(dirWithSep,false)
  def exportToHtmlWithColours(dirWithSep: String): Node = exportToHtml(dirWithSep, true)
  def exportToHtml(dirWithSep: String, colours: Boolean): Node = <p>
  <h1>{name}</h1>
  <table border="1">
  <tr>{
    if(colours) <th>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</th>
    else ""
  }<th></th><th>Value</th></tr>
  {
    val multiplier = DataUnit.size(unit)
    val suffix = DataUnit.symbol(unit)
    (0 until chart.length)map{ i=>
      <tr>{ if(colours) <td style={"background: #"+Colours.inHex(i)}>&nbsp;</td>
      else ""
      }<td>{chart.index(i)}</td>
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

class Table2TableModel(val chart: Table2) extends AbstractTableModel {
  var unit: DataUnit.Value = DataUnit.Packets
  def multiplier = DataUnit.size(unit)
  def suffix = DataUnit.symbol(unit)
  
  def setUnit(unit: DataUnit.Value) {
    this.unit = unit
  }
  def getRowCount = chart.length
  def getColumnCount = 2
  override def getColumnName(i:Int) = i match {
    case 0 => ""
    case 1 => "Value"
  } 
  def getValueAt(j:Int, i:Int) = try{
    i match {
      case 0 => chart.index(j)
      case 1 => if(multiplier == 1L) {
        chart.get(j).toString+suffix
      } else {
        "%1.2f".format(chart.get(j).toDouble/multiplier)+suffix
      }
    }
  } catch {
    case _ => 
      "Index out of bounds: "+(i,j)
  }
  override def isCellEditable(x:Int, y: Int) = false
  override def setValueAt(o: AnyRef, x: Int, y: Int) = ()
}
