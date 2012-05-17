package pl.umk.mat.stasiu88.nfserver.configuration
import scala.io.Source
import scala.xml.XML
import pl.umk.mat.stasiu88.nfserver.datasource.DataSourceComponent
import pl.umk.mat.stasiu88.nfserver.datasource.DirectoryDataSource
import pl.umk.mat.stasiu88.nfserver.datasource.DirectoryDataSource
import pl.umk.mat.stasiu88.nfserver.datasource.FileDataSource
import pl.umk.mat.stasiu88.nfserver.datasource.RandomDataSource
import java.io.IOException
import pl.umk.mat.stasiu88.nfserver.datasource.UnionDataSource
import pl.umk.mat.stasiu88.nfserver.authz.SimpleAuthzSource
import pl.umk.mat.stasiu88.nfserver.messages.Credentials
import pl.umk.mat.stasiu88.nfserver.authz.TrivialAuthzSource
import pl.umk.mat.stasiu88.nfserver.authz.AuthzSourceComponent
import pl.umk.mat.stasiu88.nfserver.Logging

trait XmlConfiguration extends ConfigurationComponent 
                       with    DataSourceComponent
                       with    AuthzSourceComponent 
                       with    Logging {
  private[this] lazy val document = XML.load(getClass().getResource("/config.xml"))
  log_trace(document.toString)
  log_trace((document \ "port").text.trim)
  lazy val httpPort = try {
    (document \ "port").text.trim.toInt
  } catch {
    case _ =>
      log_fatal("Invalid port configuration")
      throw new IOException("Invalid port configuration")
  }
  lazy val dataSource = {
    val sources = (document \ "datasource").flatMap{
      _.child.collect{
        case <directory>{path}</directory> => new DirectoryDataSource(path.text)
        case <file>{path}</file> => new FileDataSource(path.text)
        case <random/> => new RandomDataSource(100000)
        case i:scala.xml.Elem => 
          log_fatal("Invalid datasource configuration: "+i)
          throw new IOException("Invalid datasource configuration: "+i)
      }
    }
    val ds  = sources.length match {
      case 0 => 
        log_fatal("Empty datasource configuration")
        throw new IOException("Empty datasource configuration")
      case 1 => sources(0)
      case _ => new UnionDataSource(sources : _*)
    }
    log_info("Using as datasource: "+ds)
    
    ds
  }
  lazy val authzSource = {
    val sources = (document \ "authzsource").flatMap{
      _.child.collect {
        case <list>{list@_*}</list> =>
          new SimpleAuthzSource(
              list collect {
                case <user>{contents@_*}</user> =>
                  val m = contents. collect{
                    case <name>{username}</name> => 0 -> username.text
                    case <password>{password}</password> => 1 -> password.text
                    case x:scala.xml.Elem => 
                      log_fatal("Invalid user data: "+x)
                      throw new IOException("Invalid user data: "+x)
                  }.toMap
                  new Credentials(m(0), m(1))
              } toSet
          )
        case <allow-all/> => TrivialAuthzSource
        case i:scala.xml.Elem => 
          log_fatal("Invalid authorization source configuration: "+i)
          throw new IOException("Invalid authorization source configuration")
      }
    }
    val as = sources.length match {
      case 1 => sources(0)
      case _ =>
        log_fatal("Invalid authorization source configuration: multiple sources")
        throw new IOException("Invalid authorization source configuration: multiple sources")
    }
    log_info("Using as authorization source: "+as)
    as
  }
}
