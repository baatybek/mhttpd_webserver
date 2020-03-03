Simple Web Framework in Scala/Java.

The ContextHandler trait is  the base type for all context handlers and supports the following:

* get all the server parameters

* get / set all the application parameters

* get / set all the session parameters life-cycle methods for initialization and shutdown (allowing the handler to gracefully terminate)

* handle requests, allowing one to:
    * get all request headers
    * get requested URI
    * get all request parameters (parsed GET query parameters)
    * get request method (currently only GET is supported)
    * get remote client address information
    * set result code
    * set response headers
    * send resulting body 
    
The StaticHandler is a simple web server that can serve static pages and list directories, which extends ContextHandler

The DynamicHandler is a simple message board application, which extends ContextHandler. A message board application allows user to leave messages to one another.

To run the application, you must have: 

* scala, sbt, java.

To run the application from console:

The mhttpd_2.13-1.0.0-SNAPSHOT.jar can be built using sbt package. The resulting jar file will be placed in target/scala-2.13/mhttpd_2.13-1.0.0-SNAPSHOT.jar.

    *   scala <path/to/mhttpd_2.13-1.0.0-SNAPSHOT.jar> -c <path/to/configuration_file> [-d]
        *   -c <configuration_file> is the path to a configuration file
        *   -d turns on debugging messages to be displayed on standard output