// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import java.nio.file.Paths
import scala.sys.process._


Global / scalaVersion := "3.0.0"
val sharedSettings = Seq(version := "1.0.0-SNAPSHOT",
  organization := "bon.jo",
  scalaVersion := "3.0.0",
  libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    scalacOptions ++= Seq("-deprecation", "-feature"),
  //    scalacOptions ++=    Seq("-new-syntax","-rewrite")
)

// or any other Scala version >= 2.11.12
//    .jvmSettings(libraryDependencies += "org.scala-js" %%% "scalajs-stubs" % "1.0.0" % "provided")
lazy val `phy-shared` = {
// select supported platforms

  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure).in(file("."))  // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)



}
// configure Scala-Native settings
// .nativeSettings(/* ... */) // defined in sbt-scala-native




