// DO NOT EDIT THIS FILE
// IT IS AUTOGENERATED BY `sbtgen.sc` SCRIPT
// ALL CHANGES WILL BE LOST

////////////////////////////////////////////////////////////////////////////////

addSbtPlugin("io.7mind.izumi.sbt" % "sbt-izumi" % "0.0.100")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % PV.sbt_pgp)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % PV.sbt_scoverage)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % PV.sbt_mima_version)

// Ignore scala-xml version conflict between scoverage where `coursier` requires scala-xml v2
// and scoverage requires scala-xml v1 on Scala 2.12,
// introduced when updating scoverage from 1.9.3 to 2.0.5
libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
