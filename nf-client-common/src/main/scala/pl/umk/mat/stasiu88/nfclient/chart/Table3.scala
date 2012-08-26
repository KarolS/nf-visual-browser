/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.chart

/**
 * Chart model for data that are best suited for a 3-column table.
 * <br>
 * Model wykresu dla danych, które najlepiej pasują do trójkolumnowej tabeli.
 */
class Table3(periods: Array[String], indices: Array[String], values: Array[Long])extends Chart{
  def length = periods.length
  def index(i: Int) = indices(i)
  def period(i: Int) = periods(i)
  def get(i: Int) = values(i)
}