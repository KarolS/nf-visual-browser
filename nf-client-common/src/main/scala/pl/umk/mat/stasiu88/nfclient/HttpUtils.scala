/*
 * Copyright (c) 2011,2012 Karol M.Stasiak <karol.m.stasiak@gmail.com>
 * This software is licensed under European Union Public Licence v.1.1 or later
 */

package pl.umk.mat.stasiu88.nfclient
import java.net.HttpURLConnection
import org.apache.http.impl.client.DefaultHttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.methods.HttpPost
import org.apache.http.client.methods.HttpUriRequest
import org.apache.http.NameValuePair
import org.apache.http.message.BasicNameValuePair
import org.apache.http.client.entity.UrlEncodedFormEntity
import java.util.{ArrayList => JList}
import scala.io.Source
import scala.xml.{Node, XML}
import scalaz._
import Scalaz._

/**
 * HTTP client.
 * <br>
 * Klient HTTP.
 */
class SHttpClient {
  private[this] val client = new DefaultHttpClient
  
  /**
   * Checks if given URI responds with OK HTTP status to GET method.
   * <code>Some(true)</code> means success, 
   * <code>Some(false)</code> means server error,
   * and <code>None</code> means server is unreachable.
   * <br>
   * Sprawdza, czy podany URI odpowiada statusem HTTP OK na metodę GET.
   * <code>Some(true)</code> oznacza sukces, 
   * <code>Some(false)</code> oznacza błąd serwera,
   * a <code>None</code> oznacza, że serwer jest niedostępny.
   */
  def checkStatus(uri: String): Option[Boolean] = {
    try{
      val query = new HttpGet(uri)
      val response = client.execute(query)
      Some(response.getStatusLine.getStatusCode == 200)
    }
    catch{
      case _ => None
    }
  }
  
  /**
   * Send a POST request to given URI and optionally converts it to XML depending on the status code.
   * Returns <code>Left</code> for plaintext responses and <code>Right</code> for XML responses.
   * <br>
   * Wysyła żądanie POST na podany URI i opcjonalnie przeształca je na XML w zależności od statusu.
   * Zwraca <code>Left</code> dla odpowiedzi zwykłym tekstem i <code>Right</code> dla odpowiedzi XML. 
   */
  def request(
        uri: String, params: Map[String, String]
      )(
        xmlOnStatus:(Int)=>Boolean = {_=>false}
      ):(Int, Either[Validation[String, String],Validation[String, Node]]) = {
    val query = new HttpPost(uri)
    val nameValuePairs = new JList[NameValuePair](params.size)
    for((k,v) <- params){
      nameValuePairs.add(new BasicNameValuePair(k,v))
    }
    query.setEntity(new UrlEncodedFormEntity(nameValuePairs));
    val response = client.execute(query)
    val statusCode = response.getStatusLine.getStatusCode
    (
        statusCode, 
        if(xmlOnStatus(statusCode)){
          Right(
           response.getEntity.getContent match{
             case null => 
               failure("Empty response")
             case is =>
              val source = Source.fromInputStream(is)
              var string = "Error when reading from stream"
              try{
                string = source.mkString
                val s = XML.loadString(string)
                success(s)
              }
              catch{
                case e:Exception => 
                  failure(string+"\n"+e)
              }
              finally{
                source.close()
              }
            }
          )
        }
        else{
          Left(
           response.getEntity.getContent match{
             case null => 
               failure("Empty response")
             case is =>
               val source = Source.fromInputStream(is)
               try{
                 val s = source.mkString
                 
                 success(s)
               }catch{
                 case e:Exception =>
                   failure(e.toString)
               } finally {
                 source.close()
               }
            }
          )
        }
    )
  }
}