package NetworkService

import java.io.{DataOutputStream, PrintWriter}

class Response ( val responseCode: Int, val contentType: String, val content: Array[Byte] )  {
  def getResponseSize: Int = content.length
  def statusCode: String =
    responseCode match {
      case 200 => "HTTP/1.1 200 OK"
      case 403 => "HTTP/1.1 403 Forbidden"
      case 404 => "HTTP/1.1 404 Not Found"
      case 405 => "HTTP/1.1 405 Method Not Allowed"
      case 500 => "HTTP/1.1 500 Internal Server Error"
    }

  def getHeaders: List[String] = List[String]( statusCode, "Content-type: " + contentType, "Content-length: " + content.length )
  def sendHeaders ( writer: PrintWriter ): Unit = {
    getHeaders.foreach(writer.println)
    writer.println()
    writer.flush()
  }

  def sendContent ( dataOutputStream: DataOutputStream ): Unit =
    dataOutputStream.write( content, 0, content.length )
}

