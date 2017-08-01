name := "scala-sandbox"

scalaVersion := "2.11.8"

scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint")

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"
