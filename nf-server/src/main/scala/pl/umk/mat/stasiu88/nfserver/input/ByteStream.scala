/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.input

import java.io.BufferedInputStream
import java.io.InputStream
import java.io.IOException

object ByteStream{
  def apply(stream:InputStream, bigEndian:Boolean):ByteStream =
	if (bigEndian) new BigEndianByteStream(stream) else new LittleEndianByteStream(stream)
  def callibrate16(stream:InputStream, shortValue:Int):ByteStream = {
	val i0 = stream.read()
	val i1 = stream.read()
	if(i0 + i1*256 == shortValue) new BigEndianByteStream(stream)
	else if(i0*256 + i1 == shortValue) new LittleEndianByteStream(stream)
	else throw new IOException("Invalid file format")
  }
}

trait ByteStream{
  val stream:InputStream
  def get8():Int = stream.read()
  def get16():Int
  def get32():Int
  def get64():Long
  def skip8(count:Long=1L) {
	var left = count;
	while(left>0) {
	  // InputStream.skip(J) has a misleading name, doing a real skip
	  left -= stream.skip(left)
	}
  }
  def skip16(count:Long=1L) = skip8(2*count)
  def skip32(count:Long=1L) = skip8(4*count)
  def skip64(count:Long=1L) = skip8(8*count)
}
class BigEndianByteStream(val stream:InputStream) extends ByteStream {
  def get16():Int = {
	val i0 = stream.read()
	val i1 = stream.read()
	i0 + 0x100*i1
  }
  def get32():Int = {
	val i0 = stream.read()
	val i1 = stream.read()
	val i2 = stream.read()
	val i3 = stream.read()
	i0 + 0x100*i1 + 0x10000*i2 + 0x1000000*i3
  }
  def get64():Long = {
	val i0 = stream.read()
	val i1 = stream.read()
	val i2 = stream.read()
	val i3 = stream.read()
	val i4 = stream.read()
	val i5 = stream.read()
	val i6 = stream.read()
	val i7 = stream.read()
	0x1L*i0 +
	0x100L*i1 +
	0x10000L*i2 +
	0x1000000L*i3 +
	0x100000000L*i4 +
	0x10000000000L*i5 +
	0x1000000000000L*i6 +
	0x100000000000000L*i7
  }
}
class LittleEndianByteStream(val stream:InputStream) extends ByteStream {
  def get16():Int = {
	val i1 = stream.read()
	val i0 = stream.read()
	i0 + 0x100*i1
  }
  def get32():Int = {
	val i3 = stream.read()
	val i2 = stream.read()
	val i1 = stream.read()
	val i0 = stream.read()
	i0 + 0x100*i1 + 0x10000*i2 + 0x1000000*i3
  }
  def get64():Long = {
	val i7 = stream.read()
	val i6 = stream.read()
	val i5 = stream.read()
	val i4 = stream.read()
	val i3 = stream.read()
	val i2 = stream.read()
	val i1 = stream.read()
	val i0 = stream.read()
	0x1L*i0 +
	0x100L*i1 +
	0x10000L*i2 +
	0x1000000L*i3 +
	0x100000000L*i4 +
	0x10000000000L*i5 +
	0x1000000000000L*i6 +
	0x100000000000000L*i7
  }
}
