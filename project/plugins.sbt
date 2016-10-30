resolvers ++= Seq(
  "socrata maven" at "https://repo.socrata.com/artifactory/libs-release",
  "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
)

addSbtPlugin("com.socrata" % "socrata-sbt-plugins" %"1.6.1")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")
