ThisBuild / organization := "dev.zio"
ThisBuild / version := "3.0.7-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"

enablePlugins(JmhPlugin)

lazy val benchmarks = (project in file("."))
  .settings(
    name := "izumi-reflect-benchmarks",
    libraryDependencies ++= Seq(
      "dev.zio" %% "izumi-reflect" % "3.0.7-SNAPSHOT",
      "org.openjdk.jmh" % "jmh-core" % "1.36",
      "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.36"
    ),
    // JMH Settings
    Jmh / sourceDirectory := (Test / sourceDirectory).value,
    Jmh / classDirectory := (Test / classDirectory).value,
    Jmh / dependencyClasspath := (Test / dependencyClasspath).value,
    // Ensures we fork a new JVM for JMH
    Jmh / compile := (Jmh / compile).dependsOn(Test / compile).value,
    scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked"
    )
  )