enablePlugins(ScalaJSPlugin)

name := "Ear-training webapp"
scalaVersion := "2.11.8" // or any other Scala version >= 2.10.2

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

skip in packageJSDependencies := false
jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.5"

libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "10.0.2"
libraryDependencies += "com.thoughtworks.binding" %%% "binding" % "10.0.2"
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
