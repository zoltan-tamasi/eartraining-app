enablePlugins(ScalaJSPlugin)

name := "Ear-training webapp"
scalaVersion := "2.12.6"

scalaJSUseMainModuleInitializer := true

skip in packageJSDependencies := false
jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv()

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"

libraryDependencies += "com.thoughtworks.binding" %%% "dom" % "11.0.1"
libraryDependencies += "com.thoughtworks.binding" %%% "binding" % "11.0.1"
addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
