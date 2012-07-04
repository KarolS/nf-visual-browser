/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.messages

sealed trait JobInfo{
  def id: Symbol
}
/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class JobAccepted(id: Symbol) extends JobInfo

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class JobDone(id: Symbol) extends JobInfo

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class JobCancelled(id: Symbol) extends JobInfo

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class JobInterrupted(id: Symbol) extends JobInfo

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class JobError(id: Symbol, error: String) extends JobInfo

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class JobInProgress(id: Symbol) extends JobInfo

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class JobUnknown(id: Symbol) extends JobInfo

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case object InvalidCredentials

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case object ServerNotResponding

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case object ServerBusy

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class ServerError(error: String)

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case object ServerStatusOk

/**
 * A message from agent to UI.
 * <br>
 * Komunikat od agenta do UI.
 */
case class ForgetQuery(id: Symbol)