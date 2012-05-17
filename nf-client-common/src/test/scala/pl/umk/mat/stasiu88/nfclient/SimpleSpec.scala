package pl.umk.mat.stasiu88.nfclient

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import pl.umk.mat.stasiu88.nfclient.ui.ConsoleUIComponent
import pl.umk.mat.stasiu88.nfclient.agent.DefaultAgentComponent
import pl.umk.mat.stasiu88.nfclient.messages._
import pl.umk.mat.stasiu88.nfclient.serverconf.TestingConfiguration
import pl.umk.mat.stasiu88.nfclient.serverconf.TestingConfigurationComponent
import pl.umk.mat.stasiu88.nfserver.manager.DefaultManagerComponent
import pl.umk.mat.stasiu88.nfserver.server.HttpServer
import pl.umk.mat.stasiu88.nfserver.worker.DefaultWorkerComponent
import pl.umk.mat.stasiu88.nfserver.messages.NewJob
import pl.umk.mat.stasiu88.nfserver.messages.Credentials
import pl.umk.mat.stasiu88.nfclient.timer.TimerComponent

@RunWith(classOf[JUnitRunner])
class SimpleSpec extends FlatSpec with ShouldMatchers {
  val client = new ConsoleUIComponent with DefaultAgentComponent with TimerComponent
  val agent = client.agent
  
  val server = ( 
    new  HttpServer 
    with TestingConfigurationComponent 
    with DefaultManagerComponent 
    with DefaultWorkerComponent 
  )
  
  TestingConfiguration.allowed = Set(Credentials("admin", "pass"))
  "The server" should "start" in {
    server.startServer()
  }
  "The client" should "not crash" in {
    agent ! SetServer("http://localhost:" + TestingConfiguration.httpPort)
    agent ! SetCredentials("admin", "pass")
    client.startTimer()
    agent ! NewQuery("all; ;all;bytes")
  } 
  "The test" should "wait" in {
    Thread sleep 15000
  }
  /*"The client" should "receive responses" in {
    agent ! RefreshAllQueries
    Thread sleep 5000
  }*/
}