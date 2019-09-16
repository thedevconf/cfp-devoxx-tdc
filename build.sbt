name := "cfp-tdc"

version := "2.3-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala, SbtWeb)

includeFilter in(Assets, LessKeys.less) := "*.less"

scalaVersion := "2.11.6"

resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

libraryDependencies ++= Seq(
  cache
  ,ws
  ,filters
)

val jacksonV = "2.4.3"

val elastic4sV = "1.3.2"

val elasticSearchV = "1.3.2"

libraryDependencies ++= Seq(
  ,"redis.clients" % "jedis" % "2.1.0"
  ,"com.typesafe" %% "play-plugins-mailer" % "2.4.1"
  , "org.apache.commons" % "commons-lang3" % "3.5"
  , "commons-io" % "commons-io" % "2.4"
  , "commons-codec" % "commons-codec" % "1.10" // for new Base64 that has support for String
  , "org.ocpsoft.prettytime" % "prettytime" % "3.2.4.Final"
  , "com.github.rjeschke" % "txtmark" % "0.13" // Used for Markdown in Proposal
  //, "org.scalamock" %% "scalamock-specs2-support" % "3.0.1" % "test"
  , "com.sksamuel.elastic4s" %% "elastic4s" % elastic4sV
  , "org.elasticsearch" %% "elasticsearch" % elasticSearchV
  , "com.amazonaws" % "aws-java-sdk-s3" % "1.11.327"
    exclude ("com.fasterxml.jackson.core", "jackson-annotations")
    exclude ("com.fasterxml.jackson.core", "jackson-core")
    exclude ("com.fasterxml.jackson.core", "jackson-databind")
    exclude ("joda-time", "joda-time")
)

dependencyOverrides ++= Set(
  "com.fasterxml.jackson.core" % "jackson-annotations" % "2.6.0",
  "com.fasterxml.jackson.core" % "jackson-core" % "2.6.0",
  "com.fasterxml.jackson.core" % "jackson-databind" % "2.6.0"
)