scalaVersion := "3.6.1"
resolvers += "Sonatype Public" at "https://oss.sonatype.org/content/groups/public/"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "org.scodec" %% "scodec-bits" % "1.2.0",
  "at.favre.lib" % "bytes" % "1.6.1",
  "org.scalactic" %% "scalactic" % "3.2.18",
  "org.scalatest" %% "scalatest" % "3.2.18" % "test"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
//scalacOptions ++= Seq("-rewrite", "-source:3.4-migration")
