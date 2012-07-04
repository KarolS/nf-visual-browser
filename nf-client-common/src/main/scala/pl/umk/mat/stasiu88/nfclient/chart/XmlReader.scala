/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.chart

import scala.xml.Node
import scala.xml.Elem

/**
 * XML response parser.
 * <br>
 * Parser odpowiedzi XML.
 */
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
/**
 * Models a part of server response.
 * <br>
 * Modeluje część odpowiedzi serwera.
 */
case class Datapoint(index: String, value:Long)
/**
 * Models a part of server response.
 * <br>
 * Modeluje część odpowiedzi serwera.
 */
case class Period(timestamp: String, datapoints: List[Datapoint])
/**
 * Models a part of server response.
 * <br>
 * Modeluje część odpowiedzi serwera.
 */
case class Bucket(name: String, periods: List[Period])
/**
 * Models a part of server response.
 * <br>
 * Modeluje część odpowiedzi serwera.
 */
case class Statistic(typ: String, buckets: List[Bucket])