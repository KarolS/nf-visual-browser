package pl.umk.mat.stasiu88.nfserver.datasource

import pl.umk.mat.stasiu88.nfserver.input.DataFile
import pl.umk.mat.stasiu88.nfserver.Flow
import pl.umk.mat.stasiu88.nfserver.Utils._
import pl.umk.mat.stasiu88.nfserver.query.Query
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import java.io.File

class DirectoryDataSource(val dir:File) extends DataSource{
  def this(name: String) = this(new File(name))
  val dirname = dir.getAbsolutePath()
  
  private val files = new HashMap[String,FileDataSource]
  
  scanDir(dir)
  
  def isGood(path:String) = !path.endsWith("current") //TODO
  
  private def scanDir(dir:File){
     dir.listFiles().foreach{ item =>
       if(item.isDirectory()) scanDir(item)
       else {
         val path = item.getAbsolutePath()
         if(item.length()>0 && isGood(path)){
           val d = new FileDataSource(item)
           files += path -> d
         }
       }
     }
  }
  def fileList = files.keys
  
  private def refreshDir(dir:File){
    dir.listFiles().foreach{ item =>
       if(item.isDirectory()) refreshDir(item)
       else {
         val path = item.getAbsolutePath()
         if(item.length()>0 && isGood(path) && !files.keySet.contains(path)){
           val d = new FileDataSource(item)
           files += path -> d
         }
       }
     }
  }
  def refresh(){
    files = files filter { case (path,_) =>
      val file = new File(path)
      (  file.exists() 
      && ! file.isDirectory() 
      && file.canRead() 
      && file.length() > 0 )
    }
    refreshDir(dir)
  }
  def foreach(q:Query)(f:Flow=>Unit){
    files.values foreach { data =>
      data.foreach(q)(f)
    }
  }
  
  override def getResult(q:Query, approxThreadCount:Int)(reportProgress: Double=>Unit) = {
    val result = approxThreadCount match {
      case 1 => getResultSingleThreaded(q)(reportProgress)
      case x => 
        val step1 = timed{
          val fileList = files.values.toList zipWithIndex
          val results = Array.fill(fileList.length)(0.0)
          (fileList).par map { case (data,idx) => 
            data.getResultSingleThreaded(q){ p=>
              results(idx) = p
              reportProgress(0.9*results.sum/results.length)
            }
          }
        }
        val step2 = timed(step1 reduce {_|+|_}) 
        step2
    }
    FileRangeCache.commit()
    reportProgress(1.0)
    result
  }
}
