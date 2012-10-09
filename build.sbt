organization := "com.metatrope"

name := "Terraria Tool"

version := "1.0"

scalaVersion := "2.9.2"

// additional libraries
libraryDependencies ++= Seq(
    "org.specs2" %% "specs2" % "1.12.1" % "test", // For specs.org tests
	"org.scalatest" %% "scalatest" % "1.6.1", // scalatest
	"junit" % "junit" % "4.8" % "test->default",
	"ch.qos.logback" % "logback-classic" % "0.9.26" % "compile->default"
)

resolvers ++= Seq("snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
                    "releases"  at "http://oss.sonatype.org/content/repositories/releases")
