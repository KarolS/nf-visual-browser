package pl.umk.mat.stasiu88.nfserver.server
import org.eclipse.jetty.server.Server
import pl.umk.mat.stasiu88.nfserver.manager.Manager

class HttpServer(port:Int){

  val manager = {
    val m = new Manager()
    m.start()
    m
  }
  val server = new Server(port)
  server.setHandler(new HttpHandler(manager))
  
  def start() {
    server.start()
  }
  
  def stop() {
    server.stop()
  }
}