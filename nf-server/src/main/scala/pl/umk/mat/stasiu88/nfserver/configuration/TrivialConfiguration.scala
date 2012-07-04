/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.configuration

/**
 * Trivial HTTP server configuration, port = 8888.
 * <br>
 * Trywialna konfiguracja serwera HTTP, port = 8888.
 */
trait TrivialConfiguration extends ConfigurationComponent{
  lazy val httpPort = 8888
}