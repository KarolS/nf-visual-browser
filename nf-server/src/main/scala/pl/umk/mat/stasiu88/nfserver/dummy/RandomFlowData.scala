/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.dummy

import pl.umk.mat.stasiu88.nfserver._
import Protocols._
import scala.util.Random

/**
 * Random flow data generator.
 * <br>
 * Generator losowych danych o przepływach.
 */
class RandomFlowData(
  val localSubnet: IP4Addr,
  val subnetSize: Int,
  val startTime: Long,
  val measurementDuration: Long,
  val maxFlowDuration: Int,
  val maxFlowPacketCount: Int,
  val maxBytesPerPacket: Int,
  val popularTcpPorts: List[Int],
  val random: Random) {
  def randomLowTcpPort = popularTcpPorts(random.nextInt(popularTcpPorts.length))
  def randomHighPort = random.nextInt(0x10000) + 0x10000

  def next(): Set[Flow] = {
    val local = random.nextBoolean
    var src = IP4Addr(localSubnet.value + random.nextInt(subnetSize) + 1)
    var dst = if (local) IP4Addr(localSubnet.value + random.nextInt(subnetSize) + 1)
      else IP4Addr(0x0B000000 + random.nextInt(0x70000000))
    val stime = startTime + random.nextInt((measurementDuration/65536).toInt)*65536L
    val duration = 1 + random.nextInt(maxFlowDuration)
    val packetCount = 1 + random.nextInt(maxFlowPacketCount)
    val flowBytes1 = 1 + packetCount + random.nextInt(packetCount * maxBytesPerPacket)
    val flowBytes2 = 1 + packetCount + random.nextInt(packetCount * maxBytesPerPacket)
    val (sport, dport) = if (random.nextBoolean) (randomLowTcpPort, randomHighPort)
      else (randomHighPort, randomLowTcpPort)
    Set(
      new Flow(src, dst, dst, 
          1, 1, 
          packetCount, flowBytes1, 
          stime, stime + duration, 
          sport, dport, 0, TCP, 0, 0, 0),
      new Flow(dst, src, src, 
          1, 1, 
          packetCount, flowBytes2, 
          stime + duration, stime + 2 * duration, 
          dport, sport, 0, TCP, 0, 0, 0)
    )
  }
  def stream(): Stream[Flow] = {
    var result: Stream[Flow] = null
    val n = next()
    for (f <- n) {
      if (null eq result) result = f #:: stream
      else {
        val const_result = result
        result = f #:: const_result
      }
    }
    result
  }
}