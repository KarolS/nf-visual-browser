/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.server

import org.eclipse.jetty.server.Server
import pl.umk.mat.stasiu88.nfserver.manager.Manager
import pl.umk.mat.stasiu88.nfserver.manager.ManagerComponent
import pl.umk.mat.stasiu88.nfserver.configuration.ConfigurationComponent
import pl.umk.mat.stasiu88.nfserver.Logging

/**
 * A composable trait providing a HTTP server.
 * <br>
 * Składalna cecha dostarczająca serwera HTTP.
 */
trait HttpServer extends Logging{
  this: ManagerComponent with ConfigurationComponent => 

  val server = new Server(httpPort)
  server.setHandler(new HttpHandler(manager))
  
  /**
   * Starts the server.
   * <br>
   * Startuje serwer.
   */
  def startServer() {
    server.start()
    log_info("Server started")
  }
  
  /**
   * Stops the server.
   * <br>
   * Zatrzymuje serwer.
   */
  def stopServer() {
    server.stop()
    log_info("Server stopped")
  }
}