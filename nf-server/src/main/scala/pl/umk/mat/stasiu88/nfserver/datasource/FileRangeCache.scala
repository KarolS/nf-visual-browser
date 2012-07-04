/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfserver.datasource
import scala.collection.mutable.HashMap
import scala.xml.XML
import scala.xml.NodeSeq
import java.net.URL
import java.io.File
import java.io.FileInputStream
import pl.umk.mat.stasiu88.nfserver.Logging

/**
 * Cache for storing time window data from files in between subsequent server runs.
 * Currently unused.
 * <br>
 * Cache przechowywujące dane o oknach czasowych z plików pomiędzy kolejnymi uruchomieniami serwera.
 * Chwilowo nieużywane. 
 */
object FileRangeCache extends Logging {
  private[this] val cache = new HashMap[String,(Long,Long)].withDefaultValue((Long.MinValue, Long.MaxValue))
  private[this] var xmlFilename:String = null
  
  /**
   * Loads a file with the cache.
   * <br>
   * Wczytuje plik z cache.
   */
  def load(filename: String){
    synchronized {
      this.xmlFilename = filename
      cache.clear()
      try{
        val x = XML.load(new FileInputStream(filename))
        for(node <- x \ "cache"){
            cache(node \ "@filename" text) = (
              (node \ "@from" text).toLong,
              (node \ "@to" text).toLong
            )
        }
      } catch {
        case _ =>
          log_warn("File "+xmlFilename+" cannot be read, assuming empty cache")
      }
    }
  }
  /**
   * Updates or inserts data about a file.
   * <br>
   * Uaktualnia lub dodaje dane o pliku. 
   */
  def update(filename: String, firstSeen: Long, lastSeen: Long){
    synchronized {
      cache(filename) = (firstSeen, lastSeen)
    }
  }
  
  /**
   * Returns data about a file
   * <br>
   * Zwraca dane o pliku.
   */
  def get(filename: String) = cache(filename)
  
  /**
   * Saves cache data to disk, in the same file they were initially read from.
   * <br>
   * Zapisuje dane z cache na dysku w tym samym pliku, z którego były na początku wczytane.
   */
  def commit(){
    synchronized {
      if(xmlFilename == null){
        log_info("Commiting skipped: no cache file specified")
      }
      else{
        val xmllist = cache.map{ case (filename,(from,to)) =>
          <file name={filename} from={from.toString} to={to.toString}/>
        }.toSeq
        val xml = <cache>{NodeSeq.fromSeq(xmllist)}</cache>
        try {
          XML.save(xmlFilename, xml)
        } catch {
          case _ =>
            log_error("File cache commiting failed")
        }
      }
    }
  }
}