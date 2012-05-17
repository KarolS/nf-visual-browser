package pl.umk.mat.stasiu88.nfserver.input

import pl.umk.mat.stasiu88.nfserver.datasource.DirectoryDataSource
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.datasource.UnionDataSource
import pl.umk.mat.stasiu88.nfserver.datasource.FileDataSource
import pl.umk.mat.stasiu88.nfserver.Utils._
object ReadingAttempt {

  def main(args: Array[String]) {
    //val d = new DataFile("/media/DOKUMENTY/NF/results/nfcapd.201203110742")
    //d foreach {f => println(f)}
    val directory = new DirectoryDataSource("/media/DOKUMENTY/NF")
    
    val q = Query("20120101000000-20120131000000;;all;bytes")
    timing{
      directory.getResult(q){_=>}
    }
    timing{
      directory.getResult(q){_=>}
    }
  }

}