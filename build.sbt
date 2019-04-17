import sbt._

val datastaxCassandra = "com.datastax.cassandra" % "cassandra-driver-core" % "3.7.1"

val rojomaJsonV3            = "com.rojoma"  %% "rojoma-json-v3"             % "[3.2.0,4.0.0)"

val socrataHttpClient       = "com.socrata" %% "socrata-http-client"        % "3.11.4"
val socrataThirdPartyUtils  = "com.socrata" %% "socrata-thirdparty-utils"   % "4.0.15"

val typesafeConfig          = "com.typesafe" % "config"                     % "1.0.2"

val scalaCheck = "org.scalacheck" %% "scalacheck" % "1.12.1"

lazy val commonSettings = Seq(
  organization := "com.socrata",
  scalaVersion := "2.10.4",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.7")
)

lazy val root = (project in file(".")).
  settings(commonSettings: _*).
  settings(
    name := "geocoders",
    libraryDependencies ++= Seq(
      rojomaJsonV3,
      socrataHttpClient,
      datastaxCassandra,
      typesafeConfig,
      socrataThirdPartyUtils,
      scalaCheck % "test"
    ),
    scalacOptions <<= scalacOptions.map(_.filterNot(_ == "-Xfatal-warnings")),
    com.socrata.sbtplugins.StylePlugin.StyleKeys.styleCheck in Test := {},
    com.socrata.sbtplugins.StylePlugin.StyleKeys.styleCheck in Compile := {},
    com.socrata.sbtplugins.findbugs.JavaFindBugsPlugin.JavaFindBugsKeys.findbugsFailOnError in Test := false,
    com.socrata.sbtplugins.findbugs.JavaFindBugsPlugin.JavaFindBugsKeys.findbugsFailOnError in Compile := false
  )
