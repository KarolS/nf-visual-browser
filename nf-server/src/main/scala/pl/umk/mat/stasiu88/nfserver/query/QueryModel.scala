/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.query

import pl.umk.mat.stasiu88.nfserver._
import scalaz._
import Scalaz._
object QueryModel {
  implicit def PeriodEq: Equal[Period] = equalA
  implicit def SummableEq: Equal[Summable] = equalA
  implicit def IndexingEq: Equal[Indexing] = equalA
  implicit def StatisticEq: Equal[Statistic] = equalBy{s=>(s.backupPeriod,s.sumOver,s.indexing,s.top)}
  implicit def SplitFilterEq: Equal[SplitFilter] = equalA
  implicit def FilterEq: Equal[Filter] = equalA
 
}
