/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.input

import java.io.BufferedInputStream
import java.io.ByteArrayInputStream
import java.io.InputStream
import java.io.IOException
import pl.umk.mat.stasiu88.nfserver.Utils._
import pl.umk.mat.stasiu88.nfserver.Addr
import pl.umk.mat.stasiu88.nfserver.IP4Addr
import pl.umk.mat.stasiu88.nfserver.IP6Addr

object ByteStream {
  def apply(stream: InputStream, bigEndian: Boolean): ByteStream =
    if (bigEndian) new BigEndianByteStream(stream) else new LittleEndianByteStream(stream)
  /**
   * Reads a 16-bit word and returns a ByteStream that would read that word as specified 
   * and will read the remaining data in the stream.  
   * <br>
   * Wczytuje 16-bitowe słowo i zwraca ByteStream, który by odczytał to słowo w podany sposób,
   * a sam będzie odczytywał pozostałe dane w strumieniu.
   */
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
  /**
   * Reads a single unsigned byte.
   * <br>
   * Odczytuje pojedynczy bajt bez znaku.
   */
  def get8(): Int = {
    //voffset +=1; 
    stream.read()
  }
  /**
   * Reads an unsigned 16-bit word.
   * <br>
   * Odczytuje 16-bitowe słowo bez znaku.
   */
  def get16(): Int
  /**
   * Reads an unsigned 32-bit word.
   * <br>
   * Odczytuje 32-bitowe słowo bez znaku.
   */
  def get32(): Long
  /**
   * Reads an unsigned 64-bit word.
   * <br>
   * Odczytuje 64-bitowe słowo bez znaku.
   */
  def get64(): Long
  /**
   * Reads an IPv4 address.
   * <br>
   * Odczytuje adres IPv4.
   */
  def getIPv4(): Addr = IP4Addr(get32().toInt)
  /**
   * Reads an IPv6 address.
   * <br>
   * Odczytuje adres IPv6.
   */
  def getIPv6(): Addr
  /**
   * Skips requested number of bytes. 
   * Unlike <code>java.io.InputStream.skip</code>, this method actually works.
   * <br>
   * Pomija podaną liczbę bajtów.
   * W przeciwieństwie do <code>java.io.InputStream.skip</code>, ta metoda działa naprawdę.
   */
  def skip8(count: Long = 1L) {
    var left = count;
    //voffset += count
    while (left > 0) {
      // InputStream.skip(J) has a misleading name, doing a real skip
      left -= stream.skip(left)
    }
  }
  /**
   * Skips requested number of 16-bit words.
   * <br>
   * Pomija podaną liczbą 16-bitowych słów.
   */
  def skip16(count: Long = 1L) = skip8(2 * count)
  /**
   * Skips requested number of 32-bit words.
   * <br>
   * Pomija podaną liczbą 32-bitowych słów.
   */
  def skip32(count: Long = 1L) = skip8(4 * count)
  /**
   * Skips requested number of 64-bit words.
   * <br>
   * Pomija podaną liczbą 64-bitowych słów.
   */
  def skip64(count: Long = 1L) = skip8(8 * count)
  /**
   * Reads requested number of bytes and returns them in an array.
   * <br>
   * Odczytuje podaną liczbą bajtów i zwraca je w tablicy.
   */
  def rawBytes(count: Long) = {
    if(count>Int.MaxValue) throw new IllegalArgumentException("Reading over 2 GB at once")
    if(count<0) throw new IllegalArgumentException("Reading negative number of bytes")
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
  /**
   * Closes this stream.
   * <br>
   * Zamyka ten strumień.
   */
  def close() = stream.close()
  
  /**
   * Returns a new ByteStream based on a given InputStream with the same endianness.
   * <br>
   * Zwraca nowy ByteStream oparty o podany InputStream o tej samej kolejności bajtów w słowach.
   */
  def getSimilar(is: InputStream): ByteStream
  
  /**
   * (Experimental) 
   * Reads a block of given size, decompressed it and returns a new ByteStream to read the decompressed data.
   * <br>
   * (Eksperymentalne)
   * Odczytuje block danego rozmiaru, dekompresuje go i zwraca nowy ByteStream służący do odczytu rozkompresowanych danych.
   */
  def decompressBlock(count: Long, lzo: Lzo) = getSimilar(new ByteArrayInputStream(
    lzo.decompress(rawBytes(count)) ))
}

/**
 * Big-Endian ByteStream
 */
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
  def getSimilar(is: InputStream) = new BigEndianByteStream(is)
}
/**
 * Little-Endian ByteStream
 */
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
  def getSimilar(is: InputStream) = new LittleEndianByteStream(is)
}
