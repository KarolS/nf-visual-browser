/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.messages

import pl.umk.mat.stasiu88.nfserver.worker.Result
import scalaz._
import Scalaz._

/**
 * An request from manager to worker to start a new job with given query.
 * <br>
 * Żądanie od menedżera do wykonawcy, by zaczął nowe zadanie z podanym zapytaniem.
 */
case class StartNewJob(
    id: Symbol, 
    query: String
    )

/**
 * An request from manager to worker to cancel a job.
 * <br>
 * Żądanie od menedżera do wykonawcy, by anulował zadanie.
 */
case class CancelJob(
    id:Symbol
    )

/**
 * An request from manager to worker to stop all jobs.
 * <br>
 * Żądanie od menedżera do wykonawcy, by zatrzymał wszystkie zadania.
 */
case object ShutdownWorker

/**
 * An request from worker to manager to update job status with its results.
 * <br>
 * Żądanie od wykonawcy do menedżera, by uaktualnił status zadania jego wynikami.
 */
case class ReportJobEnding(
    id: Symbol,
    result: Validation[Throwable, Result]
    )

/**
 * An request from worker to manager to update job progress info.
 * <br>
 * Żądanie od wykonawcy do menedżera, by uaktualnił informacje o postępie zadania.
 */
case class ReportJobProgress(
    id: Symbol,
    progress: Double
    )