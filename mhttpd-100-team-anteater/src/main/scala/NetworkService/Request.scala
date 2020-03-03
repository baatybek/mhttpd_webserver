package NetworkService

import java.io.{BufferedReader, InputStreamReader}
import java.net.Socket
import java.util.Date

/**
 *  object RequestMethod: is an enum that has http request type
 *  for current project, it's required to have just GET and POST requests, anything else is invalid
 */
object RequestMethod extends Enumeration {
  val GET, POST, Invalid = Value
}

/**
 * class ClientRequestInfo stores data related to request from a remote client
 * @param clientIPAddress is the IP address of the client (remote host) which made the request to the server
 * @param requestDateTime is is the time that the request was received
 * @param requestParams is the request line from the client
 */
class ClientRequestInfo ( val clientIPAddress: String, val requestDateTime: Date, val requestParams: String )

/**
 * Class Request handles receiving requests of the client (remote host)
 * @param clientSocket is socket of the remote host
 */
class Request ( clientSocket: Socket ) {
  private val clientIPAddress: String = getRemoteClientIPAddress
  private val reader: BufferedReader =  new BufferedReader(new InputStreamReader(clientSocket.getInputStream))
  private val requestParams: String = reader.readLine()
  private val requestDateTime: Date = new Date()
  private val requestHeaders = readHeaders
  private def getRemoteClientIPAddress: String = if( clientSocket != null && clientSocket.getRemoteSocketAddress != null ) clientSocket.getRemoteSocketAddress.toString else "0.0.0.0"
  //  private def getRequestDateAndTime: String =  new SimpleDateFormat("dd/MM/yyyy:HH:mm:ss Z").format(new Date())
  private def readHeaders: List[String] = {
    val line: String = reader.readLine()
    if( line == "" )  Nil
    else line::readHeaders
  }

  def getClientRequestInfo: ClientRequestInfo = new ClientRequestInfo( clientIPAddress, requestDateTime, requestParams )
  def getRequestHeaders: List[String] = requestHeaders
  def getAllRequestParams: List[String] = {
    if( requestParams != null ) requestParams.split( " " ).toList
    else  Nil
  }
  def getRequestedURL: Option[String] = {
    def inner ( requestParam: List[String] ): Option[String] = {
      requestParam match {
        case Nil => None
        case head::tail => if( head.startsWith("/") ) Some(head) else inner( tail )
      }
    }
    if( requestParams != null ) inner( getAllRequestParams  )
    else None
  }
  def getRequestMethod: RequestMethod.Value = {
    def inner ( requestParams: List[String] ): RequestMethod.Value =
      requestParams match {
        case head::tail => if( head.toUpperCase() == "GET" ) RequestMethod.GET
        else if ( head.toUpperCase() == "POST" ) RequestMethod.POST
        else inner( tail )
        case Nil => RequestMethod.Invalid
      }
    inner( getAllRequestParams )
  }


}