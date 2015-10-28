resolvers ++= Seq(
  "socrata maven" at "https://repository-socrata-oss.forge.cloudbees.com/release",
  "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
)

addSbtPlugin("com.socrata" % "socrata-sbt-plugins" %"1.4.3")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")
