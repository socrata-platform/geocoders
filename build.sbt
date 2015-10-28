import sbt._

val rojomaJsonV3 = "com.rojoma" %% "rojoma-json-v3" % "[3.2.0,4.0.0)"
val rojomaSimpleArm = "com.rojoma" %% "simple-arm" % "1.2.0"

val socrataHttpVersion = "3.3.2"
val socrataHttpClient = "com.socrata" %% "socrata-http-client" % socrataHttpVersion

def astyanaxExcludes(x: ModuleID) = x exclude ("commons-logging", "commons-logging") exclude ("org.mortbay.jetty", "servlet-api") exclude ("javax.servlet", "servlet-api")
val astyanaxVersion =  "1.56.48"
val astyanaxCassandra = astyanaxExcludes("com.netflix.astyanax" % "astyanax-cassandra" % astyanaxVersion)
val astyanaxThrift = astyanaxExcludes("com.netflix.astyanax" % "astyanax-thrift" % astyanaxVersion)


lazy val commonSettings = Seq(
  organization := "com.socrata",
  version := "0.1.0",
  scalaVersion := "2.10.4"
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "geocoders",
    libraryDependencies ++= Seq(
      rojomaJsonV3,
      socrataHttpClient,
      astyanaxCassandra,
      astyanaxThrift
    )
  )
