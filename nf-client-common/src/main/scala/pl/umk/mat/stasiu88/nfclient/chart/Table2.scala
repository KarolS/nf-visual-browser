/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.chart

/**
 * Chart model for data that are best suited for a 2-column table.
 * <br>
 * Model wykresu dla danych, które najlepiej pasują do dwukolumnowej tabeli.
 */
class Table2(indexes: Array[String], values: Array[Long])extends Chart{
  def length = indexes.length
  def get(i: Int) = values(i)
  def index(i: Int) = indexes(i)
}
