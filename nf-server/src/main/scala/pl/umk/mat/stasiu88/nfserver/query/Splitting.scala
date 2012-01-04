/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver.Flow
import scalaz._
import Scalaz._

object SplittingResults extends Enumeration {
  type SplittingResult = Value
  val ACCEPTED, REJECTED, IGNORED = Value
}
import SplittingResults._

sealed trait SplitFilter {
  val bucketCount: Int
  def bucketNo: Int
  def bucketNo_=(i: Int): Unit
  def classifyWithCallback(flow: Flow, callback: (Int, Flow) => Unit): SplittingResult
  def classify(flow: Flow): Option[Int] = {
    var result: Option[Int] = None
    classifyWithCallback(flow, { (x, _) => result = Some(x) })
    result
  }
  def *(that: SplitFilter) = that match {
    case CartesianProductSplitFilter(xs) => CartesianProductSplitFilter(this :: xs)
    case _                               => CartesianProductSplitFilter(List(this, that))
  }
}
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
}
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
}