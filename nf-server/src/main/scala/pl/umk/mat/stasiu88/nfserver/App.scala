package pl.umk.mat.stasiu88.nfserver
import pl.umk.mat.stasiu88.nfserver.server.HttpServer
import scala.io.Source
import scala.xml.XML
/**
 * Hello world!
 *
 */
object App {
  var x: Flow = null

  def main(args: Array[String]) = {
  	val m = XML.load(getClass().getResource("/config.xml"))
  	println(m)
    new HttpServer(8888).start()
  }
}
