/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver

import org.joda.time.Instant

/**
 * A single NetFlow flow record. Objects of this class are mutable.
 * <br>
 * Pojedynczy rekord z przepływem NetFlow. Obiekty tej klasy są mutowalne.
 */
final class Flow(
  var srcaddr: Addr = IP4Addr.ZERO,
  var destaddr: Addr = IP4Addr.ZERO,
  var nextHop: Addr = IP4Addr.ZERO,
  var inputInterface: Int = 0,
  var outputInterface: Int = 0,
  var packets: Long = 0,
  var bytes: Long = 0,
  var startTime: Long = 0,
  var endTime: Long = 0,
  var srcport: Int = 0,
  var destport: Int = 0,
  var tcpFlags: Int = 0,
  var protocol: Int = 0,
  var srcAS: Int = 0,
  var destAS: Int = 0,
  var tos: Int = 0
  ) {
  override def toString() = (
    Protocols.name(protocol) + " " +
    srcaddr + ":"+srcport + " -> " +
    destaddr + ":" + destport + " " +
    new Instant(startTime) + " - " +
    new Instant(endTime) + " " +
    bytes + "B " +
    packets + " packets"
  ) 
}
