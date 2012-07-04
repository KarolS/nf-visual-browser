/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.chart

/**
 * Chart model for cumulative charts for given buckets over given periods.
 * <br>
 * Model wykresu skumulowanego dla danych kategorii podzcas danych okresów.
 */
class CumulativeChart(categories: Array[String], periods: Array[String], values: Array[Array[Long]]) extends Chart{
  def periodCount = periods.length
  def categoryCount = categories.length
  def category(i: Int) = categories(i)
  def period(i: Int) = periods(i)

  def get(category: Int, period: Int) = values(category)(period)
  
  lazy val max = (0 until periods.length) map { p =>
    (0 until categories.length) map { cat =>
      get(cat,p)
    } sum
  } max
}