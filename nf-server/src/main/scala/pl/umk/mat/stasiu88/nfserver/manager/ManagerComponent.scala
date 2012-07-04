/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.manager
import scala.actors.Actor
import pl.umk.mat.stasiu88.nfserver.worker.WorkerComponent
import pl.umk.mat.stasiu88.nfserver.worker.Worker
import pl.umk.mat.stasiu88.nfserver.authz.AuthzSource
import pl.umk.mat.stasiu88.nfserver.authz.AuthzSourceComponent

/**
 * 
 */
trait ManagerComponent {
  this: WorkerComponent with AuthzSourceComponent =>
  def manager:Manager
  // required so it works:
  manager.init(this.authzSource, this.worker)
  manager.start()
}
trait Manager extends Actor{
  def init(authSource: AuthzSource, worker: Worker)
}