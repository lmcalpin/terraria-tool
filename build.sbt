organization := "com.metatrope"

name := "Terraria Tool"

version := "1.0"

scalaVersion := "2.9.0"

// additional libraries
libraryDependencies ++= Seq(
	"org.scala-tools.testing" % "specs_2.9.0" % "1.6.8" % "test", // For specs.org tests
	"org.scalatest" % "scalatest_2.9.0" % "1.6.1", // scalatest
	"junit" % "junit" % "4.8" % "test->default",
	"ch.qos.logback" % "logback-classic" % "0.9.26" % "compile->default"
)