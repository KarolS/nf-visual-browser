/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

/**
 * Protocol numbers.
 * <br>
 * Numbery protokołów.
 */
object Protocols {
  val TCP = 6
  val UDP = 17
  val ICMP = 1
  val IGMP = 2
  val EGP = 8
  val RSVP = 46
  val ICMP6 = 58
  val SCTP = 132
  /**
   * Converts protocol number to its name.
   * <br>
   * Konwertuje numer protokołu do jego nazwy.
   */
  def name(id: Int) = id match{
    case 6 => "TCP"
    case 17 => "UDP"
    case 1 => "ICMP"
    case 2 => "IGMP"
    case 8 => "EGP"
    case 46 => "RSVP"
    case 58 => "ICMP6"
    case 132 => "SCTP"
    case x => "protocol "+x 
  }
}
