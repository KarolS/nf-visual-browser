/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.messages

/**
 * A message to agent.
 * <br>
 * Komunikat do agenta.
 */
case class SetCredentials(username: String, password: String)

/**
 * A message to agent.
 * <br>
 * Komunikat do agenta.
 */
case class SetServer(uri: String)

/**
 * A message to agent.
 * <br>
 * Komunikat do agenta.
 */
case class NewQuery(query: String)

/**
 * A message to agent.
 * <br>
 * Komunikat do agenta.
 */
case class CancelQuery(id: Symbol)

/**
 * A message to agent.
 * <br>
 * Komunikat do agenta.
 */
case class RefreshQuery(id: Symbol)

/**
 * A message to agent.
 * <br>
 * Komunikat do agenta.
 */
case object RefreshAllQueries

/**
 * A message to agent.
 * <br>
 * Komunikat do agenta.
 */
case object CheckStatus