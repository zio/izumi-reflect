lazy val docs = project
  .in(file("izumi-reflect-docs"))
  .settings(
    publish / skip := true,
    moduleName := "izumi-reflect-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings",
    projectName := "izumi-reflect",
    mainModuleName                             := "izumi-reflect",
    projectStage                               := ProjectStage.ProductionReady,
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio"            % V.zio,
      "dev.zio" %% "zio-sbt-source" % V.zio_sbt_source
    ),
  )
  .dependsOn(LocalProject("izumi-reflectJVM"))
  .enablePlugins(WebsitePlugin)