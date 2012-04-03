package pl.umk.mat.stasiu88.nfserver.datasource
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.Flow

class UnionDataSource(val sources: DataSource*) extends DataSource{
  def foreach(q:Query)(f: Flow=>Unit){
    sources foreach { source =>
      source.foreach(q)(f)
    }
  }
  override def getResult(q:Query, approxThreadCount:Int) = approxThreadCount match {
    case 1 => getResultSingleThreaded(q)
    case x => 
      val thsPerSource = (approxThreadCount+sources.length-1)/sources.length
      sources.par map { data => 
        data.getResult(q, thsPerSource) 
      } reduce {_|+|_}
  }
}