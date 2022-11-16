// https://www.scala-js.org/
addSbtPlugin("org.scala-js" % "sbt-scalajs" % PV.scala_js_version)

// https://github.com/portable-scala/sbt-crossproject
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % PV.sbt_crossproject_version)

// https://scalacenter.github.io/scalajs-bundler/
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.21.1")

// https://github.com/scala-js/jsdependencies
addSbtPlugin("org.scala-js" % "sbt-jsdependencies" % "1.0.2")

////////////////////////////////////////////////////////////////////////////////

addSbtPlugin("io.7mind.izumi.sbt" % "sbt-izumi" % "0.0.96")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % PV.sbt_pgp)

addSbtPlugin("org.scoverage" % "sbt-scoverage" % PV.sbt_scoverage)

addSbtPlugin("com.typesafe" % "sbt-mima-plugin" % PV.sbt_mima_version)

addSbtPlugin("dev.zio" % "zio-sbt-website" % "0.0.0+83-40726325-SNAPSHOT")

resolvers += Resolver.sonatypeRepo("public")
