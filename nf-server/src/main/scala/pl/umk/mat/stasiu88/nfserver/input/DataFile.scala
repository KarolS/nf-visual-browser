/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.input

import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import org.joda.time.Instant
import pl.umk.mat.stasiu88.nfserver.Flow
import pl.umk.mat.stasiu88.nfserver.Utils._
import pl.umk.mat.stasiu88.nfserver.IP4Addr

class DataFile(fileName: String) {
  
  def debug(s: =>String){
    //println(s)
  }
  
  debug("Reading " + fileName)
  
  private val files = new BufferedInputStream(new FileInputStream(new File(fileName)))
  private val stream = ByteStream.callibrate16(files, 0xA50C)
  stream.get16() // has to be 1, ignore for now

  private val flags = stream.get32()
  debug("flags = " + flags)
  private val numBlocks = stream.get32()
  debug("numBlocks = " + numBlocks)
  private val compressed = (flags & 0x1) != 0
  if (compressed) throw new IOException("Compressed files not supported")
  private val extendedStats = (flags & 0x2) != 0
  debug("extendedStats = " + extendedStats)
  val IDENTLEN = 128
  private val fileident = stream.string(IDENTLEN)
  debug("fileident = " + fileident)
  
  private var vflowCount = 0L;
  private var vbyteCount = 0L;
  private var vpacketCount = 0L;
  private var firstSeenS = 0L
  private var lastSeenS = 0L
  private var firstSeenMs = 0
  private var lastSeenMs = 0
  if (extendedStats == false) {
    vflowCount = stream.get64()
    vbyteCount = stream.get64()
    vpacketCount = stream.get64()
    stream.skip64(12)
    firstSeenS = stream.get32()
    lastSeenS = stream.get32()
    firstSeenMs = stream.get16()
    lastSeenMs = stream.get16()
    stream.skip32()
    // expected offset: 0x114
  }
  else{
    val headerType = stream.get16()
    val statsSize = stream.get16()
    headerType match {
      case 2 => 
        
    }
    stream.skip8(statsSize)
  }
  def flowCount = vflowCount
  def byteCount = vbyteCount
  def packetCount = vpacketCount
  
  val firstSeen = firstSeenS*1000L + firstSeenMs
  val lastSeen = lastSeenS*1000L + lastSeenMs
  
  debug("flowCount = " + flowCount)
  debug("firstSeen = " + new Instant(firstSeen))
  debug("lastSeen = " + new Instant(lastSeen))
  
  def decompress(data: Array[Byte]):ByteStream = ???
  
  def foreach(function: Flow=>Unit){
    try {
      val flow = new Flow()
      var extensionMap = Map[Int,List[Int]]()
      val lzo = new Lzo
      numBlocks times {
        val numRecords = stream.get32()
        debug("numRecords = " + numRecords + " 0x" + numRecords.toHexString)
        val blockSize = stream.get32() //important iff compressed
        debug("blockSize = " + blockSize + " 0x" + blockSize.toHexString)
        stream.skip16(2)
        val records = if(compressed) stream.decompressBlock(blockSize,lzo) else stream
        // in the first record, the offset should be 0x120
        numRecords times { 
          val typ = records.get16()
          val totalSize = records.get16()
          typ match {
            case 2 => 
              var extensions = List[Int]()
              val id = records.get16()
              val exsize = records.get16()
              (exsize/2) times {
                extensions :+= records.get16() //TODO: optimize
              }
              debug("Extension map read: " + id + "->" + extensions)
              extensionMap += (id -> extensions)
            case 1 => 
              val flags = records.get8()
              val tag = records.get8()
              val extensions = extensionMap.getOrElse(records.get16(), Set()) //TODO is it OK?
              val firstMs = records.get16()
              val lastMs = records.get16()
              val firstS = records.get32()
              val lastS = records.get32()
              flow.startTime = firstS*1000L + firstMs
              flow.endTime = lastS*1000L + lastMs 
              val fwd_status = records.get8()
              flow.tcpFlags = records.get8()
              flow.protocol = records.get8()
              val src_tos = records.get8()
              flow.srcport = records.get16()
              flow.destport = records.get16()
              
              //extension 1
              if((flags & 1) == 0){
                flow.srcaddr = records.getIPv4()
                flow.destaddr = records.getIPv4()
              }
              else {
                flow.srcaddr = records.getIPv6()
                flow.destaddr = records.getIPv6()
              }
              
              //extension 2
              if((flags & 2) == 0) flow.packets = records.get32()
              else flow.packets = records.get64()
              
              //extension 3
              if((flags & 4) == 0) flow.bytes = records.get32()
              else flow.bytes = records.get64()
              
              extensions foreach {
                case 0 => ()
                case 4 => 
                  flow.inputInterface = records.get16()
                  flow.outputInterface = records.get16()
                case 5 => 
                  flow.inputInterface = records.get32().toInt
                  flow.outputInterface = records.get32().toInt
                case 6 => 
                  flow.srcAS = records.get16()
                  flow.destAS = records.get16()
                case 7 => 
                  flow.srcAS = records.get32().toInt
                  flow.destAS = records.get32().toInt
                case 8 =>
                  val dst_tos = records.get8()
                  val dir = records.get8()
                  val srcmask = records.get8()
                  val destmask = records.get8()
                  //TODO
                case 9 =>
                  flow.nextHop = records.getIPv4()
                case 10 =>
                  flow.nextHop = records.getIPv6()
                case 11 =>
                  val bgp_next_ip = records.getIPv4()
                case 12 =>
                  val bgp_next_ip = records.getIPv6()
                case 13 => 
                  val src_vlan = records.get16()
                  val dest_vlan = records.get16()
                case 14 =>
                  val out_pkts = records.get32()
                case 15 =>
                  val out_pkts = records.get64()
                case 16 =>
                  val out_bytes = records.get32()
                case 17 =>
                  val out_bytes = records.get64()
                case 18 =>
                  val aggr_flows = records.get32()
                case 19 =>
                  val aggr_flows = records.get64()
                case 20 =>
                  val in_src_mac = records.get64()
                  val out_dest_mac = records.get64()
                case 21 =>
                  val in_dest_mac = records.get64()
                  val out_src_mac = records.get64()
                case 22 =>
                  records.skip32(10) // MPLS labels
                case 23 => 
                  val sending_router_ip = records.getIPv4
                case 24 => 
                  val sending_router_ip = records.getIPv6
                case 25 =>
                  records.skip32()
                  /*
                  uint16_t    fill;
                  uint8_t           engine_type;
                  uint8_t           engine_id;
                   */
                  
              }
              function(flow)
              // esac 1
            case 3 =>
              ???
            case 0 =>
              ???
            case _ =>
              throw new IOException("Invalid record type id at offset 0x" + (stream.offset - 4).toHexString)
          }
        } 
      } 
    } catch {
      case e =>
        e.printStackTrace()
    }
  }
  
  def map[A](function: Flow=>A):Seq[A] = ???
  
  def close() = stream.close()
}
