/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.input
import pl.umk.mat.stasiu88.nfserver.Utils._

/** 
 * Highly experimental Lzo1X decompression implementation.
 * <br>
 * Wysoce eksperymentalna implementacja algorytmu Lzo1X.
 */
class Lzo {
  val result = new Array[Byte](1048576) //per-thread object pool //TODO: size?
  def decompress(compressed: Array[Byte]):Array[Byte] = {
    var inptr = 0
    var outptr = 0
    var outlen = 0
    var m = 0
    var t = 0
    @inline def in(index: Int) = compressed(index)&0xff
    @inline def out(index: Int) = result(index)&0xff
    @inline def verbatim(count: Int) = count times {
      result(outptr) = compressed(inptr)
      outptr += 1
      inptr +=1
    }
    @inline def duplicate(count: Int) = count times {
      result(outptr) = result(m)
      outptr += 1
      m +=1
    }
    var state = 0
    
    
    if(in(0) > 17){
      t = in(0) - 17
      inptr += 1
      if(t < 4){
        state = 6
      }
      else{
        state = 3
        verbatim(t)
      }
    }
    var outer = true
    while(outer) { //outer loop
      var pre = true
      while(pre) {
        state match {
          case 0 =>
            t = in(inptr) - 17
            inptr += 1
            if(t >= 16) {
              state = 4
              pre = false            
            }
            else if(t==0){
              while(in(inptr)==0){
                inptr += 1
                t += 255
              }
              t += 15 + in(inptr)
              inptr += 1
            }
            verbatim(t+3)
            t = 0
            state = 3
          case 3 =>
            var t = in(inptr) - 17
            inptr += 1
            if(t >= 16) {
              state = 4
              pre = false            
            }
            m = outptr - (1 + 0x0800) -
                (t >> 2) -
                (in(inptr) << 2)
            inptr += 1
            val oldinptr = inptr + 1
            inptr = m
            verbatim(3)
            inptr = oldinptr
            state = 5
            pre = false
          case 4 | 6 =>
            pre = false
          case 7 | 8 | 9 =>
            outer = false
        }//pre
        if(outer) do {
          var inner = true
          while(inner) {
            state match{
              case 0 | 4 =>
                if(t >= 64){
                  m = outptr - 1 - ((t>>2)&7) - in(inptr)*8
                  inptr += 1
                  t = (t >> 5) -1
                  state = 1
                }
                else if(t>=32){
                  t &= 31
                  if(t==0){
                    while(in(inptr)==0){
                      t += 255
                      inptr += 1
                    }
                    t += 31 + in(inptr)
                    inptr += 1
                  }
                  m = outptr - 1
                  m -=(in(inptr)>>2) + (in(inptr+1)<<6)
                  inptr += 2
                }
                else if (t>=16) {
                  m = outptr - ((t&8)<<11)
                  t &= 7
                  if(t==0){
                    while(in(inptr)==0){
                      t += 255
                      inptr += 1
                    }
                    t += 7 + in(inptr)
                  }
                  m -= (in(inptr)>>2) + (in(inptr+1)<<6)
                  inptr += 2
                  if(m == outptr) {
                    state = 2
                    inner = false
                    outer = false
                  }
                  m -= 0x4000
                }
                else{
                  m = outptr - 1
                  m -= t>>2
                  m -= in(inptr)<<2
                  result(outptr) = result(m)
                  result(outptr+1) = result(m+1)
                  outptr += 2
                  m += 1 //?
                  state = 5
                }
              case 1 => 
                t += 2
                duplicate(t)
                state = 5
              case 5 =>
                t = in(inptr-2)&3
                if(t==0) inner = false
                else state = 6
              case 6 => 
                verbatim(t)
                t = in(inptr)
                inptr += 1
            }
          }//inner
          state = 0
        } while (outer)
      }//outer
    }
    result
  }
}
