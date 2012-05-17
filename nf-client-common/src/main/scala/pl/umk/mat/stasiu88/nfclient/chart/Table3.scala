package pl.umk.mat.stasiu88.nfclient.chart

class Table3(periods: Array[String], indexes: Array[String], values: Array[Long])extends Chart{
  def length = periods.length
  def index(i: Int) = indexes(i)
  def period(i: Int) = periods(i)
  def get(i: Int) = values(i)
}