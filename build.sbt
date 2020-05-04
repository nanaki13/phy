// shadow sbt-scalajs' crossProject and CrossType from Scala.js 0.6.x

import java.nio.file.Paths
import scala.sys.process._


enablePlugins(ScalaJSPlugin)
val sharedSettings = Seq(version := "0.1.1-SNAPSHOT",
  organization := "bon.jo",
  scalaVersion := "2.13.1")
name := "phy"
// or any other Scala version >= 2.11.12

lazy val `phy-shared` =
// select supported platforms
  crossProject(JSPlatform, JVMPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)

    .jvmSettings(libraryDependencies += "org.scala-js" %%% "scalajs-stubs" % "1.0.0" % "provided")
// configure Scala-Native settings
// .nativeSettings(/* ... */) // defined in sbt-scala-native

lazy val `phy-jvm` =
// select supported platforms
  crossProject(JVMPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(libraryDependencies ++= Seq(  "org.xerial" % "sqlite-jdbc" % "3.21.0"))

   .dependsOn(`phy-shared`)
lazy val `phy-js` =
// select supported platforms
  crossProject(JSPlatform)
    .crossType(CrossType.Pure) // [Pure, Full, Dummy], default: CrossType.Full
    .settings(sharedSettings)
    .settings(libraryDependencies ++= Seq("org.scala-js" %%% "scalajs-dom" % "1.0.0", "org.scala-lang.modules" %%% "scala-xml" % "2.0.0-M1"
      , "bon.jo" %%% "html-app" % "0.1.1-SNAPSHOT"

    ))

    .settings(
      scalaJSUseMainModuleInitializer := true

    ).dependsOn(`phy-shared`) // defined in sbt-scalajs-crossproject



