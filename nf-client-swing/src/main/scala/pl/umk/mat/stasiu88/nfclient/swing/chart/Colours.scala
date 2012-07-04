/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient.swing.chart

import java.awt.Color
import scala.collection.mutable.{Map=>MMap}
import scala.math._

/**
 * Colour generator.
 * <br>
 * Generator kolorów.
 */
object Colours {

  private[this]val cache = MMap[Int, Color]().withDefault{ i =>
    val hue = (i*100+10)%360
    val v:Float = 0.5+0.4*(cos(Pi*i*0.8)) toFloat
    val hue_prim = hue/60
    val x:Float = v*(1.0 - abs((hue%120)/60.0 - 1.0)) toFloat ;
    hue_prim match {
      case 0 => new Color(v,x,0f)
      case 1 => new Color(x,v,0f)
      case 2 => new Color(0f,v,x)
      case 3 => new Color(0f,x,v)
      case 4 => new Color(x,0f,v)
      case 5 => new Color(v,0f,x)
      case _ => Color.GRAY
    }
  }
  /**
   * Returns a colour for given index.
   * <br>
   * Zwraca kolor o podanym indeksie.
   */
  def apply(i: Int) = {
    cache(i)
  }
  /**
   * Returns a colour for given index in hex.
   * <br>
   * Zwraca kolor o podanym indeksie w formie szesnastkowej.
   */
  def inHex(i:Int) = {
    val c = cache(i)
    List(c.getRed,c.getGreen,c.getBlue).map{"%02x" format _}.foldLeft("")(_+_)
  }
}

/**
 * Enumeration representing units.
 * <br>
 * Typ wyliczeniowy reprezentujący jednostki.
 */
object DataUnit extends Enumeration{
  type Unit = Value
  val Bytes, Kilobytes, Megabytes, Gigabytes, 
    Milliseconds, Seconds, Minutes, Hours, 
    Packets, Flows = Value 
  /**
   * Size of a unit in base units.
   * <br>
   * Rozmiar jednostki w jednostkach podstawowych.
   */
  def size(u: Value): Long = u match{
    case Kilobytes => 1024L
    case Megabytes => 1024L*1024L
    case Gigabytes => 1024L*1024L*1024L
    case Seconds =>    1000L
    case Minutes =>   60000L
    case Hours   => 3600000L
    case _ => 1L
  }
  /**
   * Symbol for a unit.
   * <br>
   * Symbol dla jednostki.
   */
  def symbol(u: Value): String = u match {
    case Bytes     => " B"
    case Kilobytes => " KB"
    case Megabytes => " MB"
    case Gigabytes => " GB"
    case Milliseconds =>
                    " ms"
    case Seconds => " s" 
    case Minutes => " min"
    case Hours   => " h"
    case _ => ""
  } 
  /**
   * Name for a unit.
   * <br>
   * Nazwa dla jednostki.
   */
  def name(u: Value): String = u match {
    case Bytes     => "Bytes"
    case Kilobytes => "Kilobytes"
    case Megabytes => "Megabytes"
    case Gigabytes => "Gigabytes"
    case Milliseconds =>
                    "Milliseconds"
    case Seconds => "Seconds" 
    case Minutes => "Minutes"
    case Hours   => "Hours"
    case Packets => "Packets"
    case Flows   => "Flows"
    case _ => ""
  } 
}
/**
 * Enumeration representing result data types.
 * <br>
 * Typ wyliczeniowy reprezentujący typy danych w wyniku.
 */
object DataType extends Enumeration{
  type Unit = Value
  val Bytes, Flows, Packets, Duration = Value 
  /**
   * All units of given type.
   * <br>
   * Wszystkie jednostki danego typu.
   */
  def units(t: Value) = t match {
    case Bytes => List(DataUnit.Bytes,DataUnit.Kilobytes,DataUnit.Megabytes,DataUnit.Gigabytes)
    case Flows => List(DataUnit.Flows)
    case Packets => List(DataUnit.Packets)
    case Duration => List(DataUnit.Milliseconds,DataUnit.Seconds,DataUnit.Minutes,DataUnit.Hours)
    case _ => List(DataUnit.Packets) //TODO: dumb safeguard
  } 
  def toString(t: Value) = t match {
    case Bytes => "Bytes"
    case Flows => "Flows"
    case Packets => "Packets"
    case Duration => "Duration"
    case _ => "?"
  }
  /**
   * Returns optimal step size for given scale and minimal step apparent height in pixels.
   * <br>
   * Zwraca optymalny rozmiar kroku dla danej skali i minimalnej widocznej wysokości kroku w pikselach.
   */
  def pickScaleStep(typ: Value, scale: Double, stepHeightInPixels: Int) = {
    val avUnits = typ match {
      case Bytes => List(1L -> " B", 10L -> "0 B", 100L -> "00 B", 
          1024L -> " KB", 10240L -> "0 KB", 102400L -> "00 KB", 
          1024*1024L-> " MB", 1024*10240L -> "0 MB", 1024*102400L -> "00 MB",
          1024*1024L*1024L -> " GB",1024*1024L*10240L -> "0 GB",1024*1024L*102400L -> "00 GB",
          1024*1024L*1024*1024L -> " TB",1024*1024L*1024*10240L -> "0 TB")
      case Duration => List(1L -> " ms", 10L -> "0 ms", 100L -> "00 ms", 
          1000L -> " s", 10000L -> "0 s", 60000L -> " min", 600000L -> "0 min", 
          3600000L -> " h", 36000000L -> "0 h")
      case _ => List(1L -> "", 10L -> "0", 100L -> "00", 1000L -> "000",
          10000L->"0000", 100000L->"00000",
          1000000L->"Mi", 10000000L->"0Mi", 100000000L->"00Mi")
    }
    avUnits.minBy{
      case (s, name) =>
        abs(log(s*scale)-log(stepHeightInPixels))
    }
  }
}
case class UnitListItem(display: String, unit: DataUnit.Value){
  override def toString = display
}

