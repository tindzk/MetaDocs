name := "MetaDocs"

organization := "pl.metastack"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "0.2.1"

libraryDependencies += "pl.metastack" %% "metaweb" % "0.1.0-SNAPSHOT"

libraryDependencies += "org.monifu" %% "minitest" % "0.13" % "test"

testFrameworks += new TestFramework("minitest.runner.Framework")
