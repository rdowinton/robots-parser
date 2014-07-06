name := "robots-parser"

organization := "org.rdowinton"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.1"

exportJars := true

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.0" % "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.1"
)