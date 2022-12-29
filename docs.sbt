lazy val docs = project
  .in(file("izumi-docs"))
  .settings(
    publish / skip := true,
    moduleName := "izumi-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName := "Izumi Reflect",
    badgeInfo := Some(
      BadgeInfo(
        artifact = "izumi-reflect_2.12",
        projectStage = ProjectStage.ProductionReady
      )
    ),
    docsPublishBranch := "develop",
    readmeCredits :=
      """|`izumi-reflect` has been created by [Septimal Mind](https://7mind.io) to power [Izumi Project](https://github.com/7mind/izumi),
         |as a replacement for `TypeTag` in reaction to a lack of confirmed information about the future of `scala-reflect`/`TypeTag` in Scala 3 ([Motivation](https://blog.7mind.io/lightweight-reflection.html)), and donated to ZIO.
         |
         |<p align="center">
         |  <a href="https://izumi.7mind.io/">
         |  <img width="40%" src="https://github.com/7mind/izumi/blob/develop/doc/microsite/src/main/tut/media/izumi-logo-full-purple.png?raw=true" alt="Izumi"/>
         |  </a>
         |</p>
         |""".stripMargin
  )
  .enablePlugins(WebsitePlugin)
