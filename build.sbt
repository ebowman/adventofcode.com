scalaVersion := "2.13.7"
resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.scodec" %% "scodec-bits" % "1.1.30",
  "at.favre.lib" % "bytes" % "1.5.0",
  "org.scalactic" %% "scalactic" % "3.2.10",
  "org.scalatest" %% "scalatest" % "3.2.10" % "test"
)

scalacOptions += "-unchecked"
scalacOptions += "-deprecation"
