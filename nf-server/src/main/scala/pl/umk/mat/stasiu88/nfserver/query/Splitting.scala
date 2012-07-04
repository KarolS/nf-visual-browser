/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver.Flow
import pl.umk.mat.stasiu88.nfserver.Subnet
import scalaz._
import Scalaz._

/**
 * Enumeration for splitting filter classification results.
 * <br>
 * Typ wyliczeniowy z wynikami klasyfikacji filtrów rozdzielających.
 */
object SplittingResults extends Enumeration {
  type SplittingResult = Value
  val ACCEPTED, REJECTED, IGNORED = Value
}

import SplittingResults._

/**
 * Splitting filter.
 * <br>
 * Filtr rozdzielający
 */
sealed trait SplitFilter {
  /**
   * Number of buckets belonging to this filter.
   * <br>
   * Liczba kategorii należących do tego filtra.
   */
  val bucketCount: Int
  /**
   * Ordinal number of the first bucket belonging to this filter.
   * <br>
   * Numer pierwszej kategorii należącego to tego filtra. 
   */
  def bucketNo: Int
  def bucketNo_=(i: Int): Unit
  /**
   * Tries to classify the flow to a bucket and calls the callback if successful.
   * <br>
   * Próbuje zaklasyfikować przepływ do kategorii i wywołuje callback w przypadku sukcesu.
   */
  def classifyWithCallback(flow: Flow, callback: (Int, Flow) => Unit): SplittingResult
  /**
   * Tries to classify the flow to a bucket.
   * <br>
   * Próbuje zaklasyfikować przepływ do kategorii.
   */
  def classify(flow: Flow): Option[Int] = {
    var result: Option[Int] = None
    classifyWithCallback(flow, { (x, _) => result = Some(x) })
    result
  }
  def *(that: SplitFilter) = that match {
    case CartesianProductSplitFilter(xs) => CartesianProductSplitFilter(this :: xs)
    case _                               => CartesianProductSplitFilter(List(this, that))
  }
  /**
   * Returns a splitting filter with subnet names replaced with actual subnets.
   * <br>
   * Zwraca filtr rozdzielający z nazwami podsieci zastąpionymi właściwymi podsieciami.
   */
  def replaceSubnets(subnets: Map[String, Subnet]): SplitFilter
  
  protected var fname:String=""
  /**
   * Generates the name for this filter, with prefix containing all the ancestor filters and negated elder sibling filters.
   * <br>
   * Generuje nazwę dla tego filtra, z prefiksem zawierającym wszystkich przodków i zanegowane starsze rodzeństwo.
   */
  def name(prefix: String): String
  /**
   * Returns the name for this bucket, if possible.
   * <br>
   * W miarę możliwości zwraca nazwę dla podanej kategorii.
   */
  def getNameForBucket(i: Int): Option[String]
}
/**
 * Splitting filter with subfilters. 
 * Ignores flows that don't match the filter, 
 * accepts flows that match the filter and are accepted by any of the subfilters,
 * rejects all other flows.
 * <br>
 * Filtr rozdzielający z podfiltrami.
 * Ignoruje przepływy, które nie są przepuszczane przez filtr,
 * akceptuje przepływy, które są przepuszczane przez filtr i są akceptowane przez któryś z podfiltrów,
 * odrzuca wszystkie pozostałe.
 */
case class NodeSplitFilter(filter: Filter, ruleset: List[SplitFilter]) extends SplitFilter {
  require(ruleset.length > 0)

  override def toString() = filter.toString + ruleset.mkString(" [", ", ", "]")

  private var bucket: Int = 0
  def bucketNo = bucket
  def bucketNo_=(i: Int) {
    bucket = i
    var sum = i
    ruleset foreach { r =>
      r.bucketNo = sum
      sum += r.bucketCount
    }
  }
  val bucketCount = ruleset map { _.bucketCount } sum
  def classifyWithCallback(flow: Flow, callback: (Int, Flow) => Unit): SplittingResult = {
    if (filter matches flow) {
      for (r <- ruleset) {
        val a = r.classifyWithCallback(flow, callback)
        if (a != IGNORED) return a
      }
      return REJECTED
    } else return IGNORED
  }
  def replaceSubnets(subnets: Map[String, Subnet]) = NodeSplitFilter(
    filter.replaceSubnets(subnets),
    ruleset map {_ replaceSubnets subnets}
  )
  def name(prefix: String) = {
    fname = if(prefix=="") filter.toString else if (filter == AllFilter) prefix else prefix+", "+filter
    var p = fname
    for(r<-ruleset){
      p = r.name(p)
    }
    filter match {
      case NotFilter(f) =>
        if(prefix=="") f.toString else prefix+", "+f
      case _ =>
        if(prefix=="") "not "+filter.toString else prefix+", not "+filter
    }
  }
  def getNameForBucket(i:Int) = if(i>=bucket && i<bucket+bucketCount) ruleset.map{_ getNameForBucket i}.foldl(none[String])(_|+|_) else None
}

/**
 * Splitfilter with no subfilters. Accepts flows that match the filter, rejects the rest.
 * <br>
 * Filtr rozdzielający bez podfiltrów. Akceptuje przepływy przepuszczane przez filtr, odrzuca pozostałe.
 */
case class LeafSplitFilter(filter: Filter) extends SplitFilter {

  override def toString() = filter.toString + " -> " + bucket

  val bucketCount = 1
  private var bucket: Int = 0
  def bucketNo = bucket
  def bucketNo_=(i: Int) {
    bucket = i
  }
  def classifyWithCallback(flow: Flow, callback: (Int, Flow) => Unit): SplittingResult =
    if (filter matches flow) {
      callback(bucket, flow)
      ACCEPTED
    } else IGNORED
    
  def replaceSubnets(subnets: Map[String, Subnet]) = LeafSplitFilter(
    filter.replaceSubnets(subnets)
  )
  
  def name(prefix: String) =  {
    fname = if(prefix=="") filter.toString else if (filter == AllFilter) prefix else prefix+", "+filter
    filter match {
      case NotFilter(f) =>
        if(prefix=="") f.toString else prefix+", "+f
      case _ =>
        if(prefix=="") "not "+filter.toString else prefix+", not "+filter
    }
  }
  def getNameForBucket(i:Int) = if(i==bucket) Some(fname) else None
}

case class CartesianProductSplitFilter(filters: List[SplitFilter]) extends SplitFilter {
  require(!(filters exists { _.isInstanceOf[CartesianProductSplitFilter] }))
  require(filters.length > 0)

  def bucketNo = 0
  def bucketNo_=(i: Int) {}
  val bucketCount = filters map { _.bucketCount } reduceLeft { _ * _ }

  def classifyWithCallback(flow: Flow, callback: (Int, Flow) => Unit): SplittingResult = {
    val classification = filters.map((f: SplitFilter) =>
      (f.bucketCount, f classify flow)
    ).foldLeft(some(0)){ (x: Option[Int], bc: (Int, Option[Int])) =>
      (x <**> bc._2){ _ * bc._1 + _ }
    }
    classification match {
      case None =>
        REJECTED
      case Some(x) =>
        callback(x, flow)
        ACCEPTED
    }
  }

  override def *(that: SplitFilter) = that match {
    case CartesianProductSplitFilter(xs) => CartesianProductSplitFilter(filters ++ xs)
    case _                               => CartesianProductSplitFilter(filters :+ that)
  }

  override def toString() = filters.mkString(" * ")
  
  def replaceSubnets(subnets: Map[String, Subnet]) = CartesianProductSplitFilter(
    filters map {_ replaceSubnets subnets}
  )
  
  def name(prefix: String) =  {
    fname = prefix
    filters foreach {
      _.name("")
    }
    "not "+prefix
  }
  def getNameForBucket(i:Int) = Some("UNSPECIFIED BUCKET")
  
}