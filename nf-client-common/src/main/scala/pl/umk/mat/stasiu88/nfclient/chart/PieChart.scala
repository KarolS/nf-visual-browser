/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.chart

/**
 * Chart model for data that can be displayed in a pie chart.
 * <br>
 * Model wykresu dla danych, które mogą być pokazane na wykresie kołowym.
 */
class PieChart(periods: Array[String], values: Array[Long])extends Chart {
  lazy val sum = values.sum
  def length = periods.length
  def period(i:Int) = periods(i)
  def value(i:Int) = values(i)
  def percentage(i:Int) = {
    if(sum<=0) 1.0/length
    else value(i).toDouble/sum
  }
  def asTable2 = new Table2(periods, values)
}