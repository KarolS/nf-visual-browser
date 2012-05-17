package pl.umk.mat.stasiu88.nfclient.swing.chart

import javax.swing.JPanel
import scala.xml.NodeSeq

abstract class ChartPanel extends JPanel{
  def name:String
  def exportToHtml(dirWithSep: String): NodeSeq
}