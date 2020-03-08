import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}



enablePlugins(SbtgenVerificationPlugin)

lazy val `izumi-reflect-thirdparty-boopickle-shaded` = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure).in(file("fundamentals/izumi-reflect-thirdparty-boopickle-shaded"))
  .settings(
    libraryDependencies ++= Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % V.kind_projector cross CrossVersion.full),
      compilerPlugin("com.github.ghik" % "silencer-plugin" % V.silencer cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % V.silencer % Provided cross CrossVersion.full,
      "org.scalatest" %%% "scalatest" % V.scalatest % Test,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    )
  )
  .settings(
    organization := "dev.zio",
    unmanagedSourceDirectories in Compile := (unmanagedSourceDirectories in Compile).value.flatMap {
      dir =>
       Seq(dir, file(dir.getPath + (CrossVersion.partialVersion(scalaVersion.value) match {
         case Some((2, 11)) => "_2.11"
         case Some((2, 12)) => "_2.12"
         case Some((2, 13)) => "_2.13"
         case _             => "_3.0"
       })))
    },
    scalacOptions in Compile --= Seq("-Ywarn-value-discard","-Ywarn-unused:_", "-Wvalue-discard", "-Wunused:_"),
    scalacOptions ++= Seq(
      s"-Xmacro-settings:scala-version=${scalaVersion.value}",
      s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
    ),
    testOptions in Test += Tests.Argument("-oDF"),
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.12.10") => Seq(
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
      case (_, "2.13.1") => Seq(
        "-Xlint:_,-eta-sam",
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
      case (_, _) => Seq.empty
    } },
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (false, "2.12.10") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izreflect.**"
      )
      case (false, "2.13.1") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izreflect.**"
      )
      case (_, _) => Seq.empty
    } }
  )
  .jvmSettings(
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.12.10",
      "2.13.1",
      "2.11.12"
    )
  )
  .jsSettings(
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.12.10",
      "2.13.1",
      "2.11.12"
    ),
    coverageEnabled := false,
    scalaJSModuleKind := ModuleKind.CommonJSModule
  )
  .nativeSettings(
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.11.12"
    ),
    coverageEnabled := false,
    test := {},
    test in Test := {}
  )
lazy val `izumi-reflect-thirdparty-boopickle-shadedJVM` = `izumi-reflect-thirdparty-boopickle-shaded`.jvm
lazy val `izumi-reflect-thirdparty-boopickle-shadedJS` = `izumi-reflect-thirdparty-boopickle-shaded`.js
lazy val `izumi-reflect-thirdparty-boopickle-shadedNative` = `izumi-reflect-thirdparty-boopickle-shaded`.native

lazy val `izumi-reflect` = crossProject(JVMPlatform, JSPlatform, NativePlatform).crossType(CrossType.Pure).in(file("fundamentals/izumi-reflect"))
  .dependsOn(
    `izumi-reflect-thirdparty-boopickle-shaded` % "test->compile;compile->compile"
  )
  .settings(
    libraryDependencies ++= Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % V.kind_projector cross CrossVersion.full),
      compilerPlugin("com.github.ghik" % "silencer-plugin" % V.silencer cross CrossVersion.full),
      "com.github.ghik" % "silencer-lib" % V.silencer % Provided cross CrossVersion.full,
      "org.scalatest" %%% "scalatest" % V.scalatest % Test,
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    )
  )
  .settings(
    organization := "dev.zio",
    unmanagedSourceDirectories in Compile := (unmanagedSourceDirectories in Compile).value.flatMap {
      dir =>
       Seq(dir, file(dir.getPath + (CrossVersion.partialVersion(scalaVersion.value) match {
         case Some((2, 11)) => "_2.11"
         case Some((2, 12)) => "_2.12"
         case Some((2, 13)) => "_2.13"
         case _             => "_3.0"
       })))
    },
    scalacOptions ++= Seq(
      s"-Xmacro-settings:scala-version=${scalaVersion.value}",
      s"-Xmacro-settings:scala-versions=${crossScalaVersions.value.mkString(":")}"
    ),
    testOptions in Test += Tests.Argument("-oDF"),
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.12.10") => Seq(
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
      case (_, "2.13.1") => Seq(
        "-Xlint:_,-eta-sam",
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
      case (_, _) => Seq.empty
    } },
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (false, "2.12.10") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izreflect.**"
      )
      case (false, "2.13.1") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izreflect.**"
      )
      case (_, _) => Seq.empty
    } }
  )
  .jvmSettings(
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.12.10",
      "2.13.1",
      "2.11.12"
    )
  )
  .jsSettings(
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.12.10",
      "2.13.1",
      "2.11.12"
    ),
    coverageEnabled := false,
    scalaJSModuleKind := ModuleKind.CommonJSModule
  )
  .nativeSettings(
    scalaVersion := crossScalaVersions.value.head,
    crossScalaVersions := Seq(
      "2.11.12"
    ),
    coverageEnabled := false,
    test := {},
    test in Test := {}
  )
lazy val `izumi-reflectJVM` = `izumi-reflect`.jvm
lazy val `izumi-reflectJS` = `izumi-reflect`.js
lazy val `izumi-reflectNative` = `izumi-reflect`.native

lazy val `fundamentals` = (project in file(".agg/fundamentals-fundamentals"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shadedJVM`,
    `izumi-reflect-thirdparty-boopickle-shadedJS`,
    `izumi-reflect-thirdparty-boopickle-shadedNative`,
    `izumi-reflectJVM`,
    `izumi-reflectJS`,
    `izumi-reflectNative`
  )

lazy val `fundamentals-jvm` = (project in file(".agg/fundamentals-fundamentals-jvm"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shadedJVM`,
    `izumi-reflectJVM`
  )

lazy val `fundamentals-js` = (project in file(".agg/fundamentals-fundamentals-js"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shadedJS`,
    `izumi-reflectJS`
  )

lazy val `fundamentals-native` = (project in file(".agg/fundamentals-fundamentals-native"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shadedNative`,
    `izumi-reflectNative`
  )

lazy val `izumi-reflect-root-jvm` = (project in file(".agg/.agg-jvm"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Seq(
      "2.12.10",
      "2.13.1"
    ),
    scalaVersion := crossScalaVersions.value.head
  )
  .aggregate(
    `fundamentals-jvm`
  )

lazy val `izumi-reflect-root-js` = (project in file(".agg/.agg-js"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Seq(
      "2.12.10",
      "2.13.1"
    ),
    scalaVersion := crossScalaVersions.value.head
  )
  .aggregate(
    `fundamentals-js`
  )

lazy val `izumi-reflect-root-native` = (project in file(".agg/.agg-native"))
  .settings(
    skip in publish := true,
    crossScalaVersions := Seq(
      "2.12.10",
      "2.13.1"
    ),
    scalaVersion := crossScalaVersions.value.head
  )
  .aggregate(
    `fundamentals-native`
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
    scalaVersion := "2.12.10",
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
    scalacOptions in ThisBuild += """-Xmacro-settings:scalatest-version=VExpr(V.scalatest)"""
  )
  .aggregate(
    `fundamentals`
  )
