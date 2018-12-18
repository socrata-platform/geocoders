resolvers ++= Seq(
  Resolver.url("socrata maven", url("https://repo.socrata.com/artifactory/ivy-libs-release"))(Resolver.ivyStylePatterns),
  "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/"
)

addSbtPlugin("com.socrata" % "socrata-sbt-plugins" %"1.6.8")

addSbtPlugin("io.spray" % "sbt-revolver" % "0.7.2")
