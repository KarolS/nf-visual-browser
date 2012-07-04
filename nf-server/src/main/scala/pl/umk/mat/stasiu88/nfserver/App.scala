/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

import pl.umk.mat.stasiu88.nfserver.server.HttpServer
import pl.umk.mat.stasiu88.nfserver.worker.DefaultWorkerComponent
import pl.umk.mat.stasiu88.nfserver.manager.DefaultManagerComponent
import pl.umk.mat.stasiu88.nfserver.configuration.XmlConfiguration
/**
 * The main entry point for the server
 * <br>
 * Główny punkt wejścia dla serwera
 */
object App {

  def main(args: Array[String]) = {
    val server = (
      new  HttpServer 
      with DefaultManagerComponent 
      with DefaultWorkerComponent 
      with XmlConfiguration
    )
    server.startServer()
  }
}
