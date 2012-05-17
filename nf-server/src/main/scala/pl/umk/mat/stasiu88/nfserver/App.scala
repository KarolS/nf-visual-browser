package pl.umk.mat.stasiu88.nfserver
import pl.umk.mat.stasiu88.nfserver.server.HttpServer
import scala.io.Source
import scala.xml.XML
import pl.umk.mat.stasiu88.nfserver.server.HttpServer
import pl.umk.mat.stasiu88.nfserver.manager.DefaultManagerComponent
import pl.umk.mat.stasiu88.nfserver.worker.DefaultWorker
import pl.umk.mat.stasiu88.nfserver.worker.DefaultWorkerComponent
import pl.umk.mat.stasiu88.nfserver.manager.DefaultManagerComponent
import pl.umk.mat.stasiu88.nfserver.datasource.DevDataSourceComponent
import pl.umk.mat.stasiu88.nfserver.configuration.TrivialConfiguration
import pl.umk.mat.stasiu88.nfserver.authz.TrivialAuthzSource
import pl.umk.mat.stasiu88.nfserver.configuration.XmlConfiguration
/**
 * The main entry point for the server
 *
 */
object App {

  def main(args: Array[String]) = {
    /*val server = (
      new  HttpServer 
      with DefaultManagerComponent 
      with DefaultWorkerComponent 
      with DevDataSourceComponent
      with TrivialConfiguration
      with TrivialAuthzSource
    )*/
    val server = (
      new  HttpServer 
      with DefaultManagerComponent 
      with DefaultWorkerComponent 
      with XmlConfiguration
    )
    server.startServer()
  }
}
