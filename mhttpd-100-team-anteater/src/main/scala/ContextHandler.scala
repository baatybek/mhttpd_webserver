import java.nio.file.{Files, Paths}

import ContextHandler.ContextHandlerFactory
import NetworkService.{Request, Response}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

abstract class Configuration {
  def getServerParam(name: String) : Option[String]
  def getHandlerParam(handler: String, name: String): Option[String]
  def getContextHandler(path: String): Option[String]
  def getHandlerFactory(handler: String): ContextHandlerFactory
  def getSessionParam(handler: String, name: String): Option[String]
  def setSessionParam(handler: String, name: String, value: Option[String]): Unit
}

class OptionConfiguration(options: List[ConfigOption]) extends Configuration {
  def retrieveAndConvert[T : ClassTag, K, V] (converter: T => (K, V)): Map[K, V] =
    options
      .flatMap {
        case e: T => List(e)
        case _ => Nil
      }
      .map(converter)
      .toMap

  val context: Map[String, String] = retrieveAndConvert[Context, String, String] {
    context => context.path -> context.handlerName
  }

  val handlers: Map[String, ContextHandlerFactory] = retrieveAndConvert[HandlerSpec, String, ContextHandlerFactory] {
    spec => spec.name -> ContextHandler.makeInstanceFactory( spec.name, spec.clazz, this)
  }

  val params: Map[String, String] = retrieveAndConvert[Param, String, String] {
    param => param.name -> param.value
  }

  val handlerParams: Map[String, Map[String, String]] = retrieveAndConvert[HandlerSpec, String, Map[String, String]] {
    spec => spec.name -> spec.options.map {
      param => param.name -> param.value
    }.toMap
  }

  val sessionParams: Map[String, mutable.Map[String, String]] = retrieveAndConvert[HandlerSpec, String, scala.collection.mutable.Map[String, String]] {
    spec => spec.name -> scala.collection.mutable.Map[String, String]()
  }

  override def getServerParam(name: String) : Option[String] =
    params.get(name)
  override def getHandlerParam (handler: String, name: String) : Option[String] =
    handlerParams.get(handler) match {
      case Some(params) => params.get(name)
      case None => None
    }
  override def getSessionParam(handler: String, name: String) : Option[String] =
    sessionParams.get(handler) match {
      case Some(params) => params.get(name)
      case None => None
    }
  override def setSessionParam(handler: String, name: String, value: Option[String]): Unit = {
    sessionParams.get(handler) match {
      case Some(params) => value match {
        case Some(v) => params.addOne(name, v)
        case None => params.subtractOne(name)
      }
      case None => throw new IllegalAccessException(
        "Handler " + handler + " is trying to set parameters, " +
          "but there is no such handler defined by the configuration" )
    }
  }

  override def getContextHandler (path: String): Option[String] =
    if( path.startsWith("/static/") ) {
      val requestPath = path.split("/")
      val handlerName = context.getOrElse("/" + requestPath(1), null)
      if( handlerName == null ) None
      else Option.apply(handlerName)
    } else {
      val handlerName = context.getOrElse("/", null)
      if( handlerName == null ) None
      else Option.apply(handlerName)
    }

  override def getHandlerFactory (handler: String) : ContextHandlerFactory = handlers(handler)
}

  trait ContextHandler {
    val configuration: Configuration
    val name: String
    def getName: String = name
    def getServerParam(name: String): Option[String] = configuration.getServerParam(name)
    def getHandlerParam(name: String): Option[String] = configuration.getHandlerParam( this.name, name )
    def getSessionParam(name: String): Option[String] = configuration.getSessionParam( this.name, name )

    //  def setHandlerParam(name: String, value: Option[String]): Unit = ???
    def setSessionParam(name: String, value: Option[String]): Unit = configuration.setSessionParam( this.name, name, value )
    def initialize ()
    def shutdown ()
    def handleRequest(request: Request): Option[Response]
    val docRoot: String = getServerParam("DocumentRoot" ).get
    val errorDocument: Map[Int, String] = Map[Int, String]( 403 -> getServerParam( "ErrorDocument403").get ,
                                                                    404 -> getServerParam( "ErrorDocument404").get ,
                                                                    405 -> getServerParam( "ErrorDocument405").get ,
                                                                    500 -> getServerParam( "ErrorDocument500").get )
    def errorResponse ( statusCode: Int ): Option[Response] =
      statusCode match {
        case 403 => Option.apply( new Response( 403, "text/html", Files.readAllBytes( Paths.get( errorDocument(403) ) ) ) )
        case 404 => Option.apply( new Response( 404, "text/html", Files.readAllBytes( Paths.get( errorDocument(404) ) ) ) )
        case 405 => Option.apply( new Response( 405, "text/html", Files.readAllBytes( Paths.get( errorDocument(405) ) ) ) )
        case 500 => Option.apply( new Response( 500, "text/html", Files.readAllBytes( Paths.get( errorDocument(500) ) ) ) )
      }
}

object ContextHandler {
  type ContextHandlerFactory = () => ContextHandler
  def makeInstanceFactory ( name: String, clazz: Class[ContextHandler],
                           configuration: Configuration): () => ContextHandler =
    () => clazz.getConstructor(classOf[String], classOf[Configuration]).newInstance( name, configuration )
}