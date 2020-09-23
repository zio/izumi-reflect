import com.typesafe.tools.mima.core._

enablePlugins(SbtgenVerificationPlugin)

lazy val `izumi-reflect-thirdparty-boopickle-shaded` = project.in(file("izumi-reflect/izumi-reflect-thirdparty-boopickle-shaded"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % V.scalatest % Test
    ),
    libraryDependencies ++= { if (scalaVersion.value.startsWith("2.")) Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % V.kind_projector cross CrossVersion.full),
      compilerPlugin("com.github.ghik" % "silencer-plugin" % V.silencer cross CrossVersion.full),
      "org.scala-lang.modules" %% "scala-collection-compat" % V.collection_compat % Provided,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    ) else Seq.empty }
  )
  .settings(
    organization := "dev.zio",
    unmanagedSourceDirectories in Compile += baseDirectory.value / ".jvm/src/main/scala" ,
    unmanagedResourceDirectories in Compile += baseDirectory.value / ".jvm/src/main/resources" ,
    unmanagedSourceDirectories in Test += baseDirectory.value / ".jvm/src/test/scala" ,
    unmanagedResourceDirectories in Test += baseDirectory.value / ".jvm/src/test/resources" ,
    unmanagedSourceDirectories in Compile ++= (unmanagedSourceDirectories in Compile).value.flatMap {
      dir =>
       val partialVersion = CrossVersion.partialVersion(scalaVersion.value)
       def scalaDir(s: String) = file(dir.getPath + s)
       (partialVersion match {
         case Some((2, n)) => Seq(scalaDir("_2"), scalaDir("_2." + n.toString))
         case Some((x, n)) => Seq(scalaDir("_3"), scalaDir("_" + x.toString + "." + n.toString))
         case None         => Seq.empty
       })
    },
    unmanagedSourceDirectories in Test ++= (unmanagedSourceDirectories in Test).value.flatMap {
      dir =>
       val partialVersion = CrossVersion.partialVersion(scalaVersion.value)
       def scalaDir(s: String) = file(dir.getPath + s)
       (partialVersion match {
         case Some((2, n)) => Seq(scalaDir("_2"), scalaDir("_2." + n.toString))
         case Some((x, n)) => Seq(scalaDir("_3"), scalaDir("_" + x.toString + "." + n.toString))
         case None         => Seq.empty
       })
    },
    scalacOptions in Compile --= Seq("-Ywarn-value-discard","-Ywarn-unused:_", "-Wvalue-discard", "-Wunused:_"),
    scalacOptions ++= Seq(
      s"-Xmacro-settings:scala-version=${scalaVersion.value}",
      s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
    ),
    sources in (Compile, doc) := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "0.27.0-RC1") => Seq(
      
      )
      case (_, _) => (sources in (Compile, doc)).value
    } },
    testOptions in Test += Tests.Argument("-oDF"),
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.12.12") => Seq(
        "-Xsource:2.13",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
        "-Ypartial-unification",
        "-Yno-adapted-args",
        "-Xlint:adapted-args",
        "-Xlint:by-name-right-associative",
        "-Xlint:constant",
        "-Xlint:delayedinit-select",
        "-Xlint:doc-detached",
        "-Xlint:inaccessible",
        "-Xlint:infer-any",
        "-Xlint:missing-interpolator",
        "-Xlint:nullary-override",
        "-Xlint:nullary-unit",
        "-Xlint:option-implicit",
        "-Xlint:package-object-classes",
        "-Xlint:poly-implicit-overload",
        "-Xlint:private-shadow",
        "-Xlint:stars-align",
        "-Xlint:type-parameter-shadow",
        "-Xlint:unsound-match",
        "-opt-warnings:_",
        "-Ywarn-extra-implicit",
        "-Ywarn-unused:_",
        "-Ywarn-adapted-args",
        "-Ywarn-dead-code",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Ywarn-numeric-widen",
        "-Ywarn-unused-import",
        "-Ywarn-value-discard",
        "-Ycache-plugin-class-loader:always",
        "-Ycache-macro-class-loader:last-modified"
      )
      case (_, "2.13.3") => Seq(
        "-Xlint:_,-eta-sam,-multiarg-infix,-byname-implicit",
        if (insideCI.value) "-Wconf:any:error" else "-Wconf:any:warning",
        "-Wconf:cat=optimizer:warning",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
        "-Wdead-code",
        "-Wextra-implicit",
        "-Wnumeric-widen",
        "-Woctal-literal",
        "-Wunused:_",
        "-Wvalue-discard",
        "-Ycache-plugin-class-loader:always",
        "-Ycache-macro-class-loader:last-modified"
      )
      case (_, "2.11.12") => Seq.empty
      case (_, _) => Seq(
        "-Ykind-projector",
        "-noindent",
        "-language:implicitConversions"
      )
    } },
    mimaPreviousArtifacts := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "0.27.0-RC1") => Set.empty
      case (_, _) => Set(organization.value %% name.value % "1.0.0-M2")
    } },
    scalacOptions -= "-Wconf:any:error",
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (false, "2.12.12") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (false, "2.13.3") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (_, _) => Seq.empty
    } },
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "0.27.0-RC1",
      "2.13.3",
      "2.12.12",
      "2.11.12"
    )
  )

lazy val `izumi-reflect` = project.in(file("izumi-reflect/izumi-reflect"))
  .dependsOn(
    `izumi-reflect-thirdparty-boopickle-shaded` % "test->compile;compile->compile"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % V.scalatest % Test
    ),
    libraryDependencies ++= { if (scalaVersion.value.startsWith("2.")) Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % V.kind_projector cross CrossVersion.full),
      compilerPlugin("com.github.ghik" % "silencer-plugin" % V.silencer cross CrossVersion.full),
      "org.scala-lang.modules" %% "scala-collection-compat" % V.collection_compat % Provided,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    ) else Seq.empty }
  )
  .settings(
    organization := "dev.zio",
    unmanagedSourceDirectories in Compile += baseDirectory.value / ".jvm/src/main/scala" ,
    unmanagedResourceDirectories in Compile += baseDirectory.value / ".jvm/src/main/resources" ,
    unmanagedSourceDirectories in Test += baseDirectory.value / ".jvm/src/test/scala" ,
    unmanagedResourceDirectories in Test += baseDirectory.value / ".jvm/src/test/resources" ,
    unmanagedSourceDirectories in Compile ++= (unmanagedSourceDirectories in Compile).value.flatMap {
      dir =>
       val partialVersion = CrossVersion.partialVersion(scalaVersion.value)
       def scalaDir(s: String) = file(dir.getPath + s)
       (partialVersion match {
         case Some((2, n)) => Seq(scalaDir("_2"), scalaDir("_2." + n.toString))
         case Some((x, n)) => Seq(scalaDir("_3"), scalaDir("_" + x.toString + "." + n.toString))
         case None         => Seq.empty
       })
    },
    unmanagedSourceDirectories in Test ++= (unmanagedSourceDirectories in Test).value.flatMap {
      dir =>
       val partialVersion = CrossVersion.partialVersion(scalaVersion.value)
       def scalaDir(s: String) = file(dir.getPath + s)
       (partialVersion match {
         case Some((2, n)) => Seq(scalaDir("_2"), scalaDir("_2." + n.toString))
         case Some((x, n)) => Seq(scalaDir("_3"), scalaDir("_" + x.toString + "." + n.toString))
         case None         => Seq.empty
       })
    },
    scalacOptions ++= Seq(
      s"-Xmacro-settings:scala-version=${scalaVersion.value}",
      s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
    ),
    sources in (Compile, doc) := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "0.27.0-RC1") => Seq(
      
      )
      case (_, _) => (sources in (Compile, doc)).value
    } },
    testOptions in Test += Tests.Argument("-oDF"),
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.12.12") => Seq(
        "-Xsource:2.13",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
        "-Ypartial-unification",
        "-Yno-adapted-args",
        "-Xlint:adapted-args",
        "-Xlint:by-name-right-associative",
        "-Xlint:constant",
        "-Xlint:delayedinit-select",
        "-Xlint:doc-detached",
        "-Xlint:inaccessible",
        "-Xlint:infer-any",
        "-Xlint:missing-interpolator",
        "-Xlint:nullary-override",
        "-Xlint:nullary-unit",
        "-Xlint:option-implicit",
        "-Xlint:package-object-classes",
        "-Xlint:poly-implicit-overload",
        "-Xlint:private-shadow",
        "-Xlint:stars-align",
        "-Xlint:type-parameter-shadow",
        "-Xlint:unsound-match",
        "-opt-warnings:_",
        "-Ywarn-extra-implicit",
        "-Ywarn-unused:_",
        "-Ywarn-adapted-args",
        "-Ywarn-dead-code",
        "-Ywarn-inaccessible",
        "-Ywarn-infer-any",
        "-Ywarn-nullary-override",
        "-Ywarn-nullary-unit",
        "-Ywarn-numeric-widen",
        "-Ywarn-unused-import",
        "-Ywarn-value-discard",
        "-Ycache-plugin-class-loader:always",
        "-Ycache-macro-class-loader:last-modified"
      )
      case (_, "2.13.3") => Seq(
        "-Xlint:_,-eta-sam,-multiarg-infix,-byname-implicit",
        if (insideCI.value) "-Wconf:any:error" else "-Wconf:any:warning",
        "-Wconf:cat=optimizer:warning",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
        "-Wdead-code",
        "-Wextra-implicit",
        "-Wnumeric-widen",
        "-Woctal-literal",
        "-Wunused:_",
        "-Wvalue-discard",
        "-Ycache-plugin-class-loader:always",
        "-Ycache-macro-class-loader:last-modified"
      )
      case (_, "2.11.12") => Seq.empty
      case (_, _) => Seq(
        "-Ykind-projector",
        "-noindent",
        "-language:implicitConversions"
      )
    } },
    mimaPreviousArtifacts := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "0.27.0-RC1") => Set.empty
      case (_, _) => Set(organization.value %% name.value % "1.0.0-M2")
    } },
    scalacOptions -= "-Wconf:any:error",
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (false, "2.12.12") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (false, "2.13.3") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (_, _) => Seq.empty
    } },
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "0.27.0-RC1",
      "2.13.3",
      "2.12.12",
      "2.11.12"
    )
  )

lazy val `izumi-reflect-aggregate` = (project in file(".agg/izumi-reflect-izumi-reflect-aggregate"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shaded`,
    `izumi-reflect`
  )

lazy val `izumi-reflect-aggregate-jvm` = (project in file(".agg/izumi-reflect-izumi-reflect-aggregate-jvm"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shaded`,
    `izumi-reflect`
  )

lazy val `izumi-reflect-root-jvm` = (project in file(".agg/.agg-jvm"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Seq(
      "0.27.0-RC1",
      "2.13.3",
      "2.12.12",
      "2.11.12"
    ),
    scalaVersion := crossScalaVersions.value.head
  )
  .aggregate(
    `izumi-reflect-aggregate-jvm`
  )

lazy val `izumi-reflect-root` = (project in file("."))
  .settings(
    skip in publish := true,
    publishMavenStyle in ThisBuild := true,
    scalacOptions in ThisBuild ++= Seq(
      "-encoding",
      "UTF-8",
      "-target:jvm-1.8",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-language:higherKinds",
      "-explaintypes"
    ),
    javacOptions in ThisBuild ++= Seq(
      "-encoding",
      "UTF-8",
      "-source",
      "1.8",
      "-target",
      "1.8",
      "-deprecation",
      "-parameters",
      "-Xlint:all",
      "-XDignore.symbol.file"
    ),
    scalacOptions in ThisBuild ++= Seq(
      s"-Xmacro-settings:product-version=${version.value}",
      s"-Xmacro-settings:product-group=${organization.value}",
      s"-Xmacro-settings:sbt-version=${sbtVersion.value}"
    ),
    crossScalaVersions := Nil,
    scalaVersion := "0.27.0-RC1",
    organization in ThisBuild := "dev.zio",
    sonatypeProfileName := "dev.zio",
    sonatypeSessionName := s"[sbt-sonatype] ${name.value} ${version.value} ${java.util.UUID.randomUUID}",
    publishTo in ThisBuild := 
    (if (!isSnapshot.value) {
        sonatypePublishToBundle.value
      } else {
        Some(Opts.resolver.sonatypeSnapshots)
    })
    ,
    credentials in ThisBuild += Credentials(file(".secrets/credentials.sonatype-nexus.properties")),
    homepage in ThisBuild := Some(url("https://zio.dev")),
    licenses in ThisBuild := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php")),
    developers in ThisBuild := List(
              Developer(id = "jdegoes", name = "John De Goes", url = url("http://degoes.net"), email = "john@degoes.net"),
              Developer(id = "7mind", name = "Septimal Mind", url = url("https://github.com/7mind"), email = "team@7mind.io"),
            ),
    scmInfo in ThisBuild := Some(ScmInfo(url("https://github.com/zio/izumi-reflect"), "scm:git:https://github.com/zio/izumi-reflect.git")),
    scalacOptions in ThisBuild += """-Xmacro-settings:scalatest-version=VExpr(V.scalatest)""",
    scalacOptions in ThisBuild += "-Xlint:-implicit-recursion",
    mimaBinaryIssueFilters in ThisBuild ++= Seq(
      ProblemFilters.exclude[Problem]("izumi.reflect.TagMacro.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.Tag.refinedTag"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTag.refinedType"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef#RefinementDecl.name")
    ),
    mimaFailOnProblem in ThisBuild := true,
    mimaFailOnNoPrevious in ThisBuild := false
  )
  .aggregate(
    `izumi-reflect-aggregate`
  )
