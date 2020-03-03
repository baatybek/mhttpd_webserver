import java.io.FileWriter
import java.text.SimpleDateFormat
import java.util.Date

import NetworkService.ClientRequestInfo

class Logger ( val accessPath: String, errorPath: String, val clientRequestInfo: ClientRequestInfo) {
  val dateTimeFormat = new SimpleDateFormat("[dd/MMM/yyy:HH:mm:ssZ]")
  def log( handlerName: String, responseCode: Int, responseSize: Int): Unit = {
    loggerWrite( clientRequestInfo.clientIPAddress + " - " + handlerName + " " + dateTimeFormat.format(clientRequestInfo.requestDateTime) + " \"" + clientRequestInfo.requestParams + "\" " + responseCode + " " + responseSize + "\n", accessPath )
  }
  def err( handlerName: String, exceptionType: String, exceptionMessage: String, exceptionStackTrace: String ): Unit = {
    loggerWrite( clientRequestInfo.clientIPAddress + " - " + handlerName + " " + dateTimeFormat.format(clientRequestInfo.requestDateTime) + " " +  exceptionType + " " + exceptionMessage + " " + exceptionStackTrace + "\n", errorPath )
  }
  def loggerWrite ( data: String, path: String ): Unit = {
    val fileWriter = new FileWriter( path, true )
    fileWriter.write( data )
    fileWriter.close()
  }
}