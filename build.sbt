enablePlugins(ScalaJSPlugin)

name := "scalajs-2048"
scalaVersion := "2.13.3"

scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.1.0"
libraryDependencies += "org.scalatest" %%% "scalatest" % "3.3.0-SNAP3" % Test
