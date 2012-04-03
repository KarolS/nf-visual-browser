package pl.umk.mat.stasiu88.nfserver.server

import org.eclipse.jetty.server.Handler
import org.eclipse.jetty.server.handler.AbstractHandler
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import javax.servlet.http.HttpServletResponse._
import org.eclipse.jetty.server.Request
import scala.collection.JavaConversions._
import pl.umk.mat.stasiu88.nfserver.Utils._
import pl.umk.mat.stasiu88.nfserver.query.QueryParser
import pl.umk.mat.stasiu88.nfserver.query.Query
import pl.umk.mat.stasiu88.nfserver.datasource.RandomDataSource
import pl.umk.mat.stasiu88.nfserver.manager.Manager
import pl.umk.mat.stasiu88.nfserver.messages._
import pl.umk.mat.stasiu88.nfserver.Logging

class HttpHandler(manager: Manager) extends AbstractHandler with Logging{
  
  def handle(target: String, baseRequest:Request, request: HttpServletRequest, response: HttpServletResponse){
    println(target)
    def respond(status: Int, wrongStatus: Int = SC_INTERNAL_SERVER_ERROR)(content: Map[String,Array[String]]=>Any) = {
      response.setStatus(status)
      response.setContentType("text/html;charset=utf-8")
      response.setCharacterEncoding("UTF-8")
      try{
        response.getWriter().print(content(request.getParameterMap().toMap))
      } catch { case exception=>
        response.setStatus(wrongStatus)
        response.getWriter().print("<html><title>Error status: "+wrongStatus+"</title><body><pre>")
        exception.printStackTrace(response.getWriter())
        response.getWriter().print("</pre></body></html>")
      }
      baseRequest setHandled true
    }

    baseRequest.getMethod() match {
      case "GET" => 
        target match {
          case "/WEB" => respond(SC_OK){_ => FORM}
          case "/WEBCLIENT" => respond(SC_OK){_ => CLIENT_FORM}
          case "/PARSER" => respond(SC_OK){_ => PARSER_FORM}
          case _ => respond(SC_NOT_FOUND){_ => "404"}
        }
      case "POST" =>
        target match {
          case "/new" => respond(SC_ACCEPTED){ params =>
            val username = params("username")(0)
            val password = params("password")(0).toCharArray()
            val query = params("query")(0)
            log_debug("New job request received")
            val reply = manager !? (1000,NewJob(query, Credentials(username,password))) getOrElse {
              new WithHttpResponse{
                def httpResponse = SC_GATEWAY_TIMEOUT
              }
            }
            reply match {
              case whr: WithHttpResponse => response.setStatus(whr.httpResponse)
            }
            reply match {
              case Accepted(id) => id.name
              case whr: WithHttpResponse => whr.httpResponse.toString
              case _ => "?"
            }
          }
          case "/get" => respond(SC_OK){ params =>
            log_debug("Get job request received")
            val username = params("username")(0)
            val password = params("password")(0).toCharArray()
            val id = Symbol(params("id")(0))
            val reply = manager !? (1000,GetJob(id, Credentials(username,password))) getOrElse {
              new WithHttpResponse{
                def httpResponse = SC_GATEWAY_TIMEOUT
              }
            }
            reply match {
              case whr: WithHttpResponse => response.setStatus(whr.httpResponse)
            }
            reply match {
              case Ok(result, originalQuery) => result.toXml(Query(originalQuery)).toString
              case whr: WithHttpResponse => whr.httpResponse.toString
              case _ => "?"
            }
          }
          case "/WEB" => respond(SC_OK){params=> 
              <html>
                <body>
                  Your input:
                  {
                  for (p<-params) 
                    yield <p>
                      <b>{p._1}:</b>
                      <ul>{
                          p._2.map(i=> <li>{i}</li>)
                      }</ul>
                    </p><hr/>
                  }
                </body>
              </html>
            }
          case "/PARSER" => respond(SC_OK){ params =>
            val q = Query(params("query")(0))
            <html>
              <body>
                Time window: {q.timeWindow}<br/>
                Time zone: {q.timeZone}<br/>
                Subnets: {q.subnets}<br/>
                Filter: {q.splitfilter}<br/>
                Statistics: {q.statistic}<br/>
                <pre>{
                  new RandomDataSource(300).getResult(q).toXml(q).toString
                }</pre>
              </body>
            </html>
          }
          case _ => respond(SC_NOT_FOUND){_ => "404"}
        }
    }
  } 
  
  val FORM=
        <html>
          <body>
            Hello Jetty!
            <form method="POST" action="/WEB">
              <input type="text" name="p"/><br/>
              <input type="text" name="q"/><br/>
              <input type="submit"/>
            </form>
          </body>
        </html>
  val CLIENT_FORM=
        <html>
          <body>
            Inner form
            <form method="POST" action="/new">
              <h2>NEW</h2>
              <input type="text" name="username"/><br/>
              <input type="text" name="password"/><br/>
              <input type="text" name="query"/><br/>
              <input type="submit"/>
            </form>
            <form method="POST" action="/get">
              <h2>GET</h2>
              <input type="text" name="username"/><br/>
              <input type="text" name="password"/><br/>
              <input type="text" name="id"/><br/>
              <input type="submit"/>
            </form>
          </body>
        </html>
  val PARSER_FORM=
        <html>
          <body>
            Parser Test
            <form method="POST" action="/PARSER">
              <input type="text" name="query"/>
              <input type="submit"/>
            </form>
          </body>
        </html>

}