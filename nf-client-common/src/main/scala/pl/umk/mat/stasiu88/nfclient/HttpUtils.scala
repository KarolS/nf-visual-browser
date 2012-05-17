package pl.umk.mat.stasiu88.nfclient
import java.net.HttpURLConnection
import org.apache.http.impl.client.DefaultHttpClient
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

class SHttpClient {
  private[this] val client = new DefaultHttpClient
  
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