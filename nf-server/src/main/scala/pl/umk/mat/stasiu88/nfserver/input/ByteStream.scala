/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.input

import java.io.BufferedInputStream
import java.io.InputStream
import java.io.IOException
import pl.umk.mat.stasiu88.nfserver.Utils._
import pl.umk.mat.stasiu88.nfserver.Addr
import pl.umk.mat.stasiu88.nfserver.IP4Addr
import pl.umk.mat.stasiu88.nfserver.IP6Addr

object ByteStream {
  def apply(stream: InputStream, bigEndian: Boolean): ByteStream =
    if (bigEndian) new BigEndianByteStream(stream) else new LittleEndianByteStream(stream)
  def callibrate16(stream: InputStream, shortValue: Int): ByteStream = {
    val i0 = stream.read()
    val i1 = stream.read()
    if (i0 + i1 * 256 == shortValue) new BigEndianByteStream(stream, 2L)
    else if (i0 * 256 + i1 == shortValue) new LittleEndianByteStream(stream, 2L)
    else throw new IOException("Invalid file format")
  }
  def copyCallibration(stream: InputStream, source: ByteStream) = source match {
    case _:BigEndianByteStream => new BigEndianByteStream(stream)
    case _:LittleEndianByteStream => new LittleEndianByteStream(stream)
    case _ => throw new IllegalArgumentException("Invalid source bytestream type")
  }
}

trait ByteStream {
  val stream: InputStream
  protected var voffset = 0L 
  def offset: Long = voffset
  def get8(): Int = {
    //voffset +=1; 
    stream.read()
  }
  def get16(): Int
  def get32(): Long
  def get64(): Long
  def getIPv4(): Addr = IP4Addr(get32().toInt)
  def getIPv6(): Addr
  def skip8(count: Long = 1L) {
    var left = count;
    //voffset += count
    while (left > 0) {
      // InputStream.skip(J) has a misleading name, doing a real skip
      left -= stream.skip(left)
    }
  }
  def skip16(count: Long = 1L) = skip8(2 * count)
  def skip32(count: Long = 1L) = skip8(4 * count)
  def skip64(count: Long = 1L) = skip8(8 * count)
  def rawBytes(count: Long) = {
    if(count>Int.MaxValue) throw new IllegalArgumentException("Reading over 2 GB at once")
    val icount = count.toInt
    val bs = new Array[Byte](icount)
    //voffset += icount
    if(stream.read(bs)!=icount) throw new IOException("Not enough bytes")
    bs
  }
  def string(maxLength: Long) = {
    val bs = rawBytes(maxLength)
    new String(bs,"latin1")
  }
  def close() = stream.close()
}
class BigEndianByteStream(val stream: InputStream, initialOffset: Long = 0L) extends ByteStream {
  voffset = initialOffset
  def get16(): Int = {
    //voffset += 2
    val i0 = stream.read()
    val i1 = stream.read()
    i0 + 0x100 * i1
  }
  def get32(): Long = {
    //voffset += 4
    val i0 = stream.read()
    val i1 = stream.read()
    val i2 = stream.read()
    val i3 = stream.read()
    i0 + 0x100L * i1 + 0x10000L * i2 + 0x1000000L * i3
  }
  def get64(): Long = {
    //voffset += 8
    val i0 = stream.read()
    val i1 = stream.read()
    val i2 = stream.read()
    val i3 = stream.read()
    val i4 = stream.read()
    val i5 = stream.read()
    val i6 = stream.read()
    val i7 = stream.read()
    0x1L * i0 +
      0x100L * i1 +
      0x10000L * i2 +
      0x1000000L * i3 +
      0x100000000L * i4 +
      0x10000000000L * i5 +
      0x1000000000000L * i6 +
      0x100000000000000L * i7
  }
  def getIPv6() = ???
}
class LittleEndianByteStream(val stream: InputStream, initialOffset: Long = 0L) extends ByteStream {
  voffset = initialOffset
  def get16(): Int = {
    //voffset += 2
    val i1 = stream.read()
    val i0 = stream.read()
    i0 + 0x100 * i1
  }
  def get32(): Long = {
    //voffset += 4
    val i3 = stream.read()
    val i2 = stream.read()
    val i1 = stream.read()
    val i0 = stream.read()
    i0 + 0x100L * i1 + 0x10000L * i2 + 0x1000000L * i3
  }
  def get64(): Long = {
    //voffset += 8
    val i7 = stream.read()
    val i6 = stream.read()
    val i5 = stream.read()
    val i4 = stream.read()
    val i3 = stream.read()
    val i2 = stream.read()
    val i1 = stream.read()
    val i0 = stream.read()
    0x1L * i0 +
      0x100L * i1 +
      0x10000L * i2 +
      0x1000000L * i3 +
      0x100000000L * i4 +
      0x10000000000L * i5 +
      0x1000000000000L * i6 +
      0x100000000000000L * i7
  }
  def getIPv6() = ???
}
