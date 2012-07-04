/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.worker

import scala.collection.mutable.{Map=>MMap}
import scala.collection.mutable.HashMap
import pl.umk.mat.stasiu88.nfserver.query.Query
import scalaz._
import Scalaz._

/**
 * Results of a query on a datasource.
 * <br>
 * Wyniki zapytania na źródle danych.
 */
class Result(private val contents: Vector[Map[Long,Map[List[Int],Long]]], val bucketCount:Int, val statisticCount:Int){
  def apply(bucket:Int, statistic:Int) = contents(bucket + statistic*bucketCount)
  
  /**
   * removes unnecesary results.
   * <br>
   * Usuwa zbędne wyniki.
   */
  def clipIfNeeded(q: Query) ={
    if(q.needsClipping) this.clip(q.clipSize)
    else this
  }
  /**
   * Returns a result with only <code>count</code> top results per period.
   * <br>
   * Zwraca wynik z tylko <code>count</code> największymi wynikami w okresie.
   */
  def clip(count:Int) = {
    //TODO: decide if clip is needed
    new Result(
      contents map {
        _ map {
          case (period,m) =>
            period -> (
              if(m.size==1) m
              else m.toList sortBy {-_._2} take count toMap
            )
        }
      },
      bucketCount,
      statisticCount
    )
  }
  
  /**
   * Combines two results together.
   * <br>
   * Łączy dwa wyniki.
   */
  def |+|(that: Result) = {
    require(this.bucketCount == that.bucketCount)
    require(this.statisticCount == that.statisticCount)
    val newContents = this.contents zip that.contents map {t => t._1|+|t._2}
    new Result(newContents, bucketCount, statisticCount)
  }
  def toXml(q: Query)=
    <result>
    {
        for(statistic <- (0 until statisticCount)) 
        yield <statistic><type>{q.statistic.sumOver(statistic)}</type>
        {
          for(bucket <- (0 until bucketCount)) 
          yield <bucket><name>{q.splitfilter.getNameForBucket(bucket).getOrElse("Unknown")}</name>
          {
            for((period,m)<-apply(bucket,statistic)) 
            yield <period><timestamp>{
              q.statistic.period.decode(period)
            }</timestamp>
            {
              for((index,value)<-m.toList sortBy {-_._2}) 
              yield <datapoint>
                <index>{q.statistic.indexing.decode(q, index)._1}</index>
                <value>{value}</value>
              </datapoint>
            }
            </period>
          }
          </bucket>
        }
        </statistic>
    }
    </result>
}

/**
 * Mutable of a query on a datasource.
 * <br>
 * Mutowalne wyniki zapytania na źródle danych.
 */
class MutableResult(val bucketCount:Int, val statisticCount:Int){
  private val contents = Array.fill(bucketCount*statisticCount){
    new HashMap[Long,MMap[List[Int],Long]]()
  }
  
  def apply(bucket:Int, statistic:Int) = contents(bucket + statistic*bucketCount)
  /**
   * Adds a value for given bucket, statistic, and period.
   * <br>
   * Dodaje wartość dla danej kategorii, statystyki i okresu.
   */
  def add(bucket:Int, statistic:Int, period:Long, index: List[Int], value:Long) {
    val m = apply(bucket,statistic)
    if(!m.contains(period)) m(period) = new HashMap()
    val mm = m(period)
    if(!mm.contains(index)) mm(index)=value
    else mm(index)=mm(index)+value
  }
  /**
   * Converts into immutable result.
   * <br>
   * Konwertuje na niemodyfikowalny wynik.
   */
  def freeze():Result = {
    new Result(
        Vector(contents.view.map{ m:HashMap[Long,MMap[List[Int],Long]]=>
            Map(m.view.map{ case (i:Long,mm:HashMap[List[Int],Long])=>
              i->Map(mm.view.toSeq:_*)
            }.toSeq:_*)
        }.toSeq:_*),
        bucketCount,
        statisticCount
    )
  }
}