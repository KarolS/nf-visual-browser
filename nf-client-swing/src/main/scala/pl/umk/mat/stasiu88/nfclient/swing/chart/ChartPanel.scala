/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing.chart

import javax.swing.JPanel
import scala.xml.NodeSeq

/**
 * A panel with a chart.
 * <br>
 * Panel z wykresem.
 */
abstract class ChartPanel extends JPanel{
  /**
   * Returns the name of the chart.
   * <br>
   * Zwraca nazwę wykresu.
   */
  def name:String
  /**
   * Returns a XML fragment with the data table and exports a PNG image into given directory if needed.
   * <br>
   * Zwraca fragment XML z tabelą z danymi i eksportuje obraz PNG do danego katalogu jeśli trzeba.
   */
  def exportToHtml(dirWithSep: String): NodeSeq
}