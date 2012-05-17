package pl.umk.mat.stasiu88.nfclient.chart

import scala.xml.Node
import scala.xml.Elem

object XmlReader {
  def read(node: Node):List[Statistic] = {
    (node \ "statistic").collect { case s:Elem =>
      Statistic(
        (s \ "type").text,
        (s \ "bucket").collect { case b:Elem =>
          Bucket(
            (b \ "name").text,
            (b \ "period").collect { case p:Elem =>
              Period(
                (p \ "timestamp").text,
                (p \ "datapoint").collect { case d:Elem =>
                  Datapoint(
                    (d \ "index").text,
                    (d \ "value").text.trim.toLong
                  )
                }.toList
              )
            }.toList
          )
        }.toList
      )
    }.toList
  }
}

case class Datapoint(index: String, value:Long)
case class Period(timestamp: String, datapoints: List[Datapoint])
case class Bucket(name: String, periods: List[Period])
case class Statistic(typ: String, buckets: List[Bucket])