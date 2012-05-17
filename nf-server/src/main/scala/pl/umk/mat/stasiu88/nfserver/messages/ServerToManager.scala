package pl.umk.mat.stasiu88.nfserver.messages
import java.util.Arrays
import javax.servlet.http.HttpServletResponse._
import pl.umk.mat.stasiu88.nfserver.worker.Result

case class Credentials(username: String, password: String)

/*
 * Following messages are sent from the server to the manager
 */
case class NewJob(
    query:String,
    credentials: Credentials
    )

case class GetJob(
    id: Symbol,
    credentials: Credentials
    )
case class HCancelJob(
    id: Symbol,
    credentials: Credentials
    )
case object ShutdownManager

/*
 * Following messages are sent from the manager to the server
 */

trait WithHttpResponse {
  def httpResponse: Int
} 

case class GatewayTimeout extends WithHttpResponse {
  def httpResponse = SC_GATEWAY_TIMEOUT //502
}

case class Unauthorized() extends WithHttpResponse {
  def httpResponse = SC_UNAUTHORIZED //401
}
case class Accepted(id: Symbol) extends WithHttpResponse {
  def httpResponse = SC_ACCEPTED //202
}
case class NotFound() extends WithHttpResponse {
  def httpResponse = SC_NOT_FOUND //404
}
case class ServerError(cause: Option[Throwable]) extends WithHttpResponse {
  def httpResponse = SC_INTERNAL_SERVER_ERROR //500
}
case class ServiceUnavailable() extends WithHttpResponse {
  def httpResponse = SC_SERVICE_UNAVAILABLE //503
}
case class Ok(result: Result, originalQuery: String) extends WithHttpResponse{
  def httpResponse = SC_OK //200
}
case class NoContent() extends WithHttpResponse {
  def httpResponse = SC_NO_CONTENT //204
}
case class PartialContent(progress: Double) extends WithHttpResponse {
  def httpResponse = SC_PARTIAL_CONTENT //206
}

