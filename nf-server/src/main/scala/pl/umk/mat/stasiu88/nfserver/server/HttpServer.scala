package pl.umk.mat.stasiu88.nfserver.server
import org.eclipse.jetty.server.Server
import pl.umk.mat.stasiu88.nfserver.manager.Manager
import pl.umk.mat.stasiu88.nfserver.manager.ManagerComponent
import pl.umk.mat.stasiu88.nfserver.configuration.ConfigurationComponent
import pl.umk.mat.stasiu88.nfserver.Logging

trait HttpServer extends Logging{
  this: ManagerComponent with ConfigurationComponent => 

  val server = new Server(httpPort)
  server.setHandler(new HttpHandler(manager))
  
  def startServer() {
    server.start()
    log_info("Server started")
  }
  
  def stopServer() {
    server.stop()
    log_info("Server stopped")
  }
}