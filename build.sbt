scalaVersion := "2.11.4"

scalaBinaryVersion:= CrossVersion.binaryScalaVersion("2.11.4")

crossVersion := CrossVersion.binary

organization := "ch.epfl.data"

version := "0.1"

libraryDependencies += "ch.epfl.data" % "sc-shared_2.11" % "0.1-SNAPSHOT"
