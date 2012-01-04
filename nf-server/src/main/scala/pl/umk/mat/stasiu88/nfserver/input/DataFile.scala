/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licenced under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.input

import java.io.BufferedInputStream
import java.io.File
import java.io.FileInputStream
import java.io.IOException

class DataFile(fileName: String) {
  private val files = new BufferedInputStream(new FileInputStream(new File(fileName)))
  private val stream = ByteStream.callibrate16(files, 0xA50C)
  stream.get16() //?

  private val flags = stream.get32()
  if ((flags & 0x1) != 0) throw new IOException("Compressed files not supported")

  private val extendedStats = (flags & 0x2) != 0
  private var flowCount = 0L;
  private var firstSeen = 0
  private var lastSeen = 0
  private var firstSeenMs = 0
  private var lastSeenMs = 0
  if (extendedStats == false) {
    flowCount = stream.get64()
    stream.skip64(14)
    firstSeen = stream.get32()
    lastSeen = stream.get32()
    firstSeenMs = stream.get16()
    lastSeenMs = stream.get16()
    stream.skip32()
  }
}
