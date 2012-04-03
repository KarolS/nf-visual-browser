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
    val toRemove = new HashSet[String]()
    files.keys foreach { path =>
      val file = new File(path)
      if(!file.exists() || file.isDirectory() || !file.canRead() || file.length()==0){
        toRemove += path
      }
    }
    toRemove foreach { path =>
      files -= path
    }
    refreshDir(dir)
  }
  def foreach(q:Query)(f:Flow=>Unit){
    files.values foreach { data =>
      data.foreach(q)(f)
    }
  }
  
  override def getResult(q:Query, approxThreadCount:Int) = approxThreadCount match {
    case 1 => getResultSingleThreaded(q)
    case x => 
      val step1 = timed(files.values.par map { data => 
        data.getResultSingleThreaded(q) 
      })
      val step2 = timed(step1 reduce {_|+|_}) 
      step2
  }
}