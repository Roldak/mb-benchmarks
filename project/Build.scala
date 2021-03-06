import sbt._
import Keys._

object MyBuild extends Build {

  scalaVersion in ThisBuild := "2.11.5"
  
  lazy val defaultSettings = Defaults.defaultSettings ++ Seq[Setting[_]](
    organization := "ch.epfl.lamp",
    version := "0.1-SNAPSHOT",
    // The plugin requires the latest version of the scalac compiler. You
    // can use older compilers, but before reporting a bug, please check
    // that it can be reproduced with the latest version of the compiler.
    scalaVersion := "2.11.5"
  )

  lazy val root = project.in(file(".")).aggregate(mbarrays)
  
  lazy val mbarrays = Project(
	"mbarrays",
	file("mbarrays"),
    settings = defaultSettings ++ miniboxingSettings
  )
  
  lazy val mergesortnomb = Project(
	"mergesort-no-mb",
	file("mergesort-no-mb"),
	settings = defaultSettings
  )
  
  lazy val mergesortmb = Project(
	"mergesort-mb",
	file("mergesort-mb"),
	settings = defaultSettings ++ miniboxingSettings
  )
  
  lazy val mergesortcompnomb = Project(
	"mergesort-comp-no-mb",
	file("mergesort-comp-no-mb"),
	settings = defaultSettings
  )
  
  lazy val mergesortcompmb = Project(
	"mergesort-comp-mb",
	file("mergesort-comp-mb"),
	settings = defaultSettings ++ miniboxingSettings
  )
  
  libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
  
  /** Settings for the miniboxing plugin */
  lazy val miniboxingSettings = Seq[Setting[_]](
    resolvers += Resolver.sonatypeRepo("snapshots"),
    libraryDependencies += "org.scala-miniboxing.plugins" %% "miniboxing-runtime" % "0.4-SNAPSHOT",
    addCompilerPlugin("org.scala-miniboxing.plugins" %% "miniboxing-plugin" % "0.4-SNAPSHOT"),
    scalacOptions ++= (
      "-P:minibox:log" ::    // enable the miniboxing plugin output
      //                       // (which explains what the plugin is doing)
      //"-P:minibox:hijack" :: // enable hijacking the @specialized annotations
      //                       // transforming them into @miniboxed annotations
      Nil
    )
  )
}
