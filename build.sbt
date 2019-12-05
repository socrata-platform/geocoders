resolvers += "socrata maven" at "https://repo.socrata.com/artifactory/libs-release"

organization := "com.socrata"

scalaVersion := "2.12.8"

crossScalaVersions := Seq("2.10.4", "2.11.7", scalaVersion.value)

mimaPreviousArtifacts := Set("com.socrata" %% "geocoders" % "2.2.1")

name := "geocoders"

libraryDependencies ++= Seq(
  "com.rojoma" %% "rojoma-json-v3" % "[3.2.0,4.0.0)",
  "com.socrata" %% "socrata-http-client" % "3.12.0",
  "com.datastax.cassandra" % "cassandra-driver-core" % "3.7.1",
  "com.typesafe" % "config" % "1.0.2",
  "com.socrata" %% "socrata-thirdparty-utils" % "5.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

scalacOptions ++= Seq("-deprecation", "-feature")

