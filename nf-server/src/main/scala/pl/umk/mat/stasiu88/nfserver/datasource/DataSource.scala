package pl.umk.mat.stasiu88.nfserver.datasource

import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Flow
import pl.umk.mat.stasiu88.nfserver.worker.Result
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.worker.MutableResult

trait DataSourceComponent {
  def dataSource: DataSource
}

trait DataSource {
  
  def foreach(query:Query)(f:Flow=>Unit):Unit
  
  def getResult(q:Query, approxThreadCount: Int=8)(reportProgress: Double=>Unit): Result = getResultSingleThreaded(q)(reportProgress)
  
  def getQuickResult(q:Query): Option[Result]= None
  
  def getResultSingleThreaded(q:Query)(reportProgress: Double=>Unit): Result = {
    getQuickResult(q) foreach { return _ }
    val result = new MutableResult(q.splitfilter.bucketCount, q.statistic.sumOver.length)
    foreach(q){ f=>
      val indexes = q.statistic.indexing(q,f)
      val period = q.statistic.period.apply(q,f)
      q.splitfilter.classify(f) foreach { bucket =>
        var i = 0
        for(v <- q.statistic.sumOver){
          for(index<-indexes){
            result.add(bucket,i,period,index,v(f))
          }
          i += 1
        }
      }
    }
    val frozenResult = result.freeze()
    reportProgress(1.0)
    frozenResult
  }
}
