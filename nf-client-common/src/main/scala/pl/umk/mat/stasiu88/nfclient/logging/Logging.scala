/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.logging

/**
 * Composable trait providing logging.
 * <br>
 * Składalna cecha dostarczająca logowania.
 */
trait Logging {
  
  def rawLog(msg: String):Unit
  
  def log_trace(msg: =>String) = rawLog("[TRACE] "+msg)
  def log_debug(msg: =>String) = rawLog("[DEBUG] "+msg)
  def log_info (msg: =>String) = rawLog("[INFO] " +msg)
  def log_warn (msg: =>String) = rawLog("[WARN] " +msg)
  def log_error(msg: =>String) = rawLog("[ERROR] "+msg)
  def log_fatal(msg: =>String) = rawLog("[FATAL] "+msg)
}

/**
 * Composable trait providing logging. Logs on the console.
 * <br>
 * Składalna cecha dostarczająca logowania. Loguje na konsoli.
 */
trait ConsoleLogging extends Logging {
  def rawLog(msg: String) = Console synchronized {
    // TODO: improve
    Console.out.println(this.getClass().getCanonicalName()+": "+msg) //TODO
    Console.out.flush()
  }
}

/**
 * Composable trait providing logging. Logs nowhere.
 * <br>
 * Składalna cecha dostarczająca logowania. Nie loguje nigdzie.
 */
trait NullLogging extends Logging {
  def rawLog(msg: String) = ()
}