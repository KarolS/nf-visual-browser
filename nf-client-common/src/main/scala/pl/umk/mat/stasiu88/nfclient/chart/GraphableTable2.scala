/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.chart

/**
 * Chart model for data that can be displayed on a bar plot.
 * <br>
 * Model wykresu dla danych, które można przedstawić na wykresie słupkowym.
 */
class GraphableTable2(periods: Array[String], values: Array[Long])extends Chart {
  def length = periods.length
  def get(i: Int) = values(i)
  def period(i: Int) = periods(i)
  
  lazy val max = values.max
  def asTable2 = new Table2(periods, values)
}