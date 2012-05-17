package pl.umk.mat.stasiu88.nfclient.chart

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