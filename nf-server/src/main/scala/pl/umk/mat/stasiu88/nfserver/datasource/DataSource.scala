package pl.umk.mat.stasiu88.nfserver.datasource

import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Flow
import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.worker.MutableResult

trait DataSource {
  
  def foreach(query:Query)(f:Flow=>Unit):Unit
  
  def getResult(q:Query, approxThreadCount: Int=8): Result = getResultSingleThreaded(q)
  
  def getQuickResult(q:Query): Option[Result]= None
  
  def getResultSingleThreaded(q:Query): Result = {
    getQuickResult(q) foreach { return _ }
    val result = new MutableResult(q.splitfilter.bucketCount, q.statistic.sumOver.length)
    foreach(q){ f=>
      q.splitfilter.classify(f) foreach { bucket =>
        val indexes = q.statistic.indexing(q,f)
        val period = q.statistic.period.apply(q,f)
        var i = 0
        for(v <- q.statistic.sumOver){
          for(index<-indexes){
            result.add(bucket,i,period,index,v(f))
          }
          i+=1
        }
      }
    }
    result.freeze()
  }
}