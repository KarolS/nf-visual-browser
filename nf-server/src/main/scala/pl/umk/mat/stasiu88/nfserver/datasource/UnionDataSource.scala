/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.datasource
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Flow

/**
 * Data source combining several data sources.
 * <br>
 * Źródło danych łączące kilka źródeł danych.
 */
class UnionDataSource(val sources: DataSource*) extends DataSource{
  def foreach(q:Query)(f: Flow=>Unit){
    sources foreach { source =>
      source.foreach(q)(f)
    }
  }
  override def getResult(q:Query, approxThreadCount:Int)(reportResult: Double=>Unit) = approxThreadCount match {
    case 1 => getResultSingleThreaded(q)(reportResult: Double=>Unit)
    case x => 
      val thsPerSource = (approxThreadCount+sources.length-1)/sources.length
      val results = Array.fill(sources.length)(0.0)
      (sources zipWithIndex).par map { case (data,idx) => 
        data.getResult(q, thsPerSource){p=>
          results(idx) = p
          reportResult(results.sum/results.length)
        }
      } reduce {_|+|_}
  }
}