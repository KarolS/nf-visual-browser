/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.configuration

/**
 * A composable trait for providing HTTP Server configuration.
 * <br>
 * Składalna cecha dostarczająca konfigurację serwera HTTP.
 */
trait ConfigurationComponent {
  /**
   * Port the servers listens at.
   * <br>
   * Port, na którym nasłuchuje serwer.
   */
  def httpPort: Int
}