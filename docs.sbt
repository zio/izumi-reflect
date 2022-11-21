
lazy val docs = project
  .in(file("izumi-docs"))
  .settings(
    publish / skip := true,
    moduleName := "izumi-docs",
    scalacOptions -= "-Yno-imports",
    scalacOptions -= "-Xfatal-warnings"
  )
  .enablePlugins(WebsitePlugin)

