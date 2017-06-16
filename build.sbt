name := "DiscreteOptimizationScala"

version := "v1.0"

scalaVersion := "2.11.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

libraryDependencies += "junit" % "junit" % "4.10" % "test"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.1.7"

libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.5.0"

resolvers += "Oscar Releases" at "http://artifactory.info.ucl.ac.be/artifactory/libs-release/"

//resolvers += Resolver.url("Oscar Releases", url("http://artifactory.info.ucl.ac.be/artifactory/libs-release/"))

libraryDependencies += "oscar" % "oscar-cp_2.11" % "3.1.0" withSources()