resolvers += "socrata maven" at "https://repo.socrata.com/artifactory/libs-release"

organization := "com.socrata"

scalaVersion := "2.12.8"

crossScalaVersions := Seq("2.10.4", "2.11.7", scalaVersion.value)

mimaPreviousArtifacts := Set("com.socrata" %% "geocoders" % "3.0.0")

name := "geocoders"

libraryDependencies ++= Seq(
  // note: any newer version of liquibase hard-links to logback rather than
  // using slf4j properly.  That would break pretty much all users of _this_
  // library because we're pretty standardized on log4j.  See
  // https://liquibase.jira.com/browse/CORE-3212
  "org.liquibase" % "liquibase-core" % "3.5.5",
  "com.rojoma" %% "rojoma-json-v3" % "[3.2.0,4.0.0)",
  "com.rojoma" %% "simple-arm-v2" % "2.3.2",
  "com.socrata" %% "socrata-http-client" % "3.12.0",
  "com.datastax.oss" % "java-driver-core" % "4.3.1",
  "com.typesafe" % "config" % "1.0.2",
  "com.socrata" %% "socrata-thirdparty-utils" % "5.0.0",
  "org.scalatest" %% "scalatest" % "3.0.8" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)

scalacOptions ++= Seq("-deprecation", "-feature")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)
