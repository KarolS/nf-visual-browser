package pl.umk.mat.stasiu88.nfclient.chart

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