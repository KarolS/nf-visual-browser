/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.worker

import scala.actors.Actor
import pl.umk.mat.stasiu88.nfserver.manager.Manager
import pl.umk.mat.stasiu88.nfserver.datasource.DataSource
import pl.umk.mat.stasiu88.nfserver.datasource.DataSourceComponent

trait WorkerComponent {
  this: DataSourceComponent =>
  def worker: Worker
  worker.init(this.dataSource)
}

trait Worker extends Actor {
  def init(dataSource: DataSource): Unit
}