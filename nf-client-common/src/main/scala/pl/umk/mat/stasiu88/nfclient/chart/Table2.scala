package pl.umk.mat.stasiu88.nfclient.chart

class Table2(indexes: Array[String], values: Array[Long])extends Chart{
  def length = indexes.length
  def get(i: Int) = values(i)
  def index(i: Int) = indexes(i)
}
