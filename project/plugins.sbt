// https://github.com/lampepfl/dotty-example-project#projectpluginssbt
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % PV.sbt_dotty_version)

////////////////////////////////////////////////////////////////////////////////

addSbtPlugin("io.7mind.izumi.sbt" % "sbt-izumi" % "0.0.76")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % PV.sbt_pgp)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % PV.sbt_scoverage)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % PV.sbt_mima_version)

