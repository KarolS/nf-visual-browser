package pl.umk.mat.stasiu88.nfclient.chart

class GraphableTable2(periods: Array[String], values: Array[Long])extends Chart {
  def length = periods.length
  def get(i: Int) = values(i)
  def period(i: Int) = periods(i)
  
  lazy val max = values.max
  def asTable2 = new Table2(periods, values)
}