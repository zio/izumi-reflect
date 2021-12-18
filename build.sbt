import com.typesafe.tools.mima.core._

enablePlugins(SbtgenVerificationPlugin)

lazy val `izumi-reflect-thirdparty-boopickle-shaded` = project.in(file("izumi-reflect/izumi-reflect-thirdparty-boopickle-shaded"))
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % V.scalatest % Test
    ),
    libraryDependencies ++= { if (scalaVersion.value.startsWith("2.")) Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % V.kind_projector cross CrossVersion.full),
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    ) else Seq.empty }
  )
  .settings(
    crossScalaVersions := Seq(
      "3.1.0",
      "2.13.7",
      "2.12.15",
      "2.11.12"
    ),
    scalaVersion := crossScalaVersions.value.head,
    organization := "dev.zio",
    Compile / unmanagedSourceDirectories += baseDirectory.value / ".jvm/src/main/scala" ,
    Compile / unmanagedSourceDirectories ++= (scalaBinaryVersion.value :: CrossVersion.partialVersion(scalaVersion.value).toList.map(_._1))
      .map(v => baseDirectory.value / s".jvm/src/main/scala-$v").distinct,
    Compile / unmanagedResourceDirectories += baseDirectory.value / ".jvm/src/main/resources" ,
    Test / unmanagedSourceDirectories += baseDirectory.value / ".jvm/src/test/scala" ,
    Test / unmanagedSourceDirectories ++= (scalaBinaryVersion.value :: CrossVersion.partialVersion(scalaVersion.value).toList.map(_._1))
      .map(v => baseDirectory.value / s".jvm/src/test/scala-$v").distinct,
    Test / unmanagedResourceDirectories += baseDirectory.value / ".jvm/src/test/resources" ,
    Compile / unmanagedSourceDirectories ++= {
      val version = scalaVersion.value
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).filter(_ <= CrossVersion.partialVersion(version)).flatten
      (Compile / unmanagedSourceDirectories).value.flatMap {
        case dir if dir.getPath.endsWith("scala") => ltEqVersions.map { case (m, n) => file(dir.getPath + s"-$m.$n+") }
        case _ => Seq.empty
      }
    },
    Test / unmanagedSourceDirectories ++= {
      val version = scalaVersion.value
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).filter(_ <= CrossVersion.partialVersion(version)).flatten
      (Test / unmanagedSourceDirectories).value.flatMap {
        case dir if dir.getPath.endsWith("scala") => ltEqVersions.map { case (m, n) => file(dir.getPath + s"-$m.$n+") }
        case _ => Seq.empty
      }
    },
    Compile / unmanagedSourceDirectories ++= {
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).sorted.flatten
      def joinV = (_: Product).productIterator.mkString(".")
      val allRangeVersions = (2 to math.max(2, ltEqVersions.size - 1))
        .flatMap(i => ltEqVersions.sliding(i).filter(_.size == i))
        .map(l => (l.head, l.last))
      CrossVersion.partialVersion(scalaVersion.value).toList.flatMap {
        version =>
          val rangeVersions = allRangeVersions
            .filter { case (l, r) => l <= version && version <= r }
            .map { case (l, r) => s"-${joinV(l)}-${joinV(r)}" }
          (Compile / unmanagedSourceDirectories).value.flatMap {
            case dir if dir.getPath.endsWith("scala") => rangeVersions.map { vStr => file(dir.getPath + vStr) }
            case _ => Seq.empty
          }
      }
    },
    Test / unmanagedSourceDirectories ++= {
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).sorted.flatten
      def joinV = (_: Product).productIterator.mkString(".")
      val allRangeVersions = (2 to math.max(2, ltEqVersions.size - 1))
        .flatMap(i => ltEqVersions.sliding(i).filter(_.size == i))
        .map(l => (l.head, l.last))
      CrossVersion.partialVersion(scalaVersion.value).toList.flatMap {
        version =>
          val rangeVersions = allRangeVersions
            .filter { case (l, r) => l <= version && version <= r }
            .map { case (l, r) => s"-${joinV(l)}-${joinV(r)}" }
          (Test / unmanagedSourceDirectories).value.flatMap {
            case dir if dir.getPath.endsWith("scala") => rangeVersions.map { vStr => file(dir.getPath + vStr) }
            case _ => Seq.empty
          }
      }
    },
    Compile / doc / sources := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "3.1.0") => Seq(
      
      )
      case (_, _) => (Compile / doc / sources).value
    } },
    Test / testOptions += Tests.Argument("-oDF"),
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.11.12") => Seq.empty
      case (_, "2.12.15") => Seq(
        "-target:jvm-1.8",
        "-explaintypes",
        "-Ypartial-unification",
        if (insideCI.value) "-Wconf:any:error" else "-Wconf:any:warning",
        "-Wconf:cat=optimizer:warning",
        "-Wconf:cat=other-match-analysis:error",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
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
        "-Ycache-macro-class-loader:last-modified",
        "-Wconf:msg=nowarn:silent"
      )
      case (_, "2.13.7") => Seq(
        "-target:jvm-1.8",
        "-explaintypes",
        if (insideCI.value) "-Wconf:any:error" else "-Wconf:any:warning",
        "-Wconf:cat=optimizer:warning",
        "-Wconf:cat=other-match-analysis:error",
        "-Vtype-diffs",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
        "-Wdead-code",
        "-Wextra-implicit",
        "-Wnumeric-widen",
        "-Woctal-literal",
        "-Wvalue-discard",
        "-Wunused:_",
        "-Wmacros:after",
        "-Ycache-plugin-class-loader:always",
        "-Ycache-macro-class-loader:last-modified",
        "-Wconf:msg=nowarn:silent"
      )
      case (_, _) => Seq(
        "-Ykind-projector",
        "-no-indent",
        "-language:implicitConversions"
      )
    } },
    scalacOptions -= "-Wconf:any:error",
    mimaPreviousArtifacts := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "3.1.0") => Set.empty
      case (_, _) => Set(organization.value %% name.value % "1.0.0")
    } },
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.13.7") => Seq(
        "-Xlint:-implicit-recursion"
      )
      case (_, _) => Seq.empty
    } },
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (false, "2.12.15") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (false, "2.13.7") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (_, _) => Seq.empty
    } },
    Compile / scalacOptions --= Seq("-Ywarn-value-discard","-Ywarn-unused:_", "-Wvalue-discard", "-Wunused:_")
  )

lazy val `izumi-reflect` = project.in(file("izumi-reflect/izumi-reflect"))
  .dependsOn(
    `izumi-reflect-thirdparty-boopickle-shaded` % "test->compile;compile->compile"
  )
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % V.scalatest % Test,
      "org.scala-lang.modules" %% "scala-collection-compat" % V.collection_compat % Provided
    ),
    libraryDependencies ++= { if (scalaVersion.value.startsWith("2.")) Seq(
      compilerPlugin("org.typelevel" % "kind-projector" % V.kind_projector cross CrossVersion.full),
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % Provided
    ) else Seq.empty }
  )
  .settings(
    crossScalaVersions := Seq(
      "3.1.0",
      "2.13.7",
      "2.12.15",
      "2.11.12"
    ),
    scalaVersion := crossScalaVersions.value.head,
    organization := "dev.zio",
    Compile / unmanagedSourceDirectories += baseDirectory.value / ".jvm/src/main/scala" ,
    Compile / unmanagedSourceDirectories ++= (scalaBinaryVersion.value :: CrossVersion.partialVersion(scalaVersion.value).toList.map(_._1))
      .map(v => baseDirectory.value / s".jvm/src/main/scala-$v").distinct,
    Compile / unmanagedResourceDirectories += baseDirectory.value / ".jvm/src/main/resources" ,
    Test / unmanagedSourceDirectories += baseDirectory.value / ".jvm/src/test/scala" ,
    Test / unmanagedSourceDirectories ++= (scalaBinaryVersion.value :: CrossVersion.partialVersion(scalaVersion.value).toList.map(_._1))
      .map(v => baseDirectory.value / s".jvm/src/test/scala-$v").distinct,
    Test / unmanagedResourceDirectories += baseDirectory.value / ".jvm/src/test/resources" ,
    Compile / unmanagedSourceDirectories ++= {
      val version = scalaVersion.value
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).filter(_ <= CrossVersion.partialVersion(version)).flatten
      (Compile / unmanagedSourceDirectories).value.flatMap {
        case dir if dir.getPath.endsWith("scala") => ltEqVersions.map { case (m, n) => file(dir.getPath + s"-$m.$n+") }
        case _ => Seq.empty
      }
    },
    Test / unmanagedSourceDirectories ++= {
      val version = scalaVersion.value
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).filter(_ <= CrossVersion.partialVersion(version)).flatten
      (Test / unmanagedSourceDirectories).value.flatMap {
        case dir if dir.getPath.endsWith("scala") => ltEqVersions.map { case (m, n) => file(dir.getPath + s"-$m.$n+") }
        case _ => Seq.empty
      }
    },
    Compile / unmanagedSourceDirectories ++= {
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).sorted.flatten
      def joinV = (_: Product).productIterator.mkString(".")
      val allRangeVersions = (2 to math.max(2, ltEqVersions.size - 1))
        .flatMap(i => ltEqVersions.sliding(i).filter(_.size == i))
        .map(l => (l.head, l.last))
      CrossVersion.partialVersion(scalaVersion.value).toList.flatMap {
        version =>
          val rangeVersions = allRangeVersions
            .filter { case (l, r) => l <= version && version <= r }
            .map { case (l, r) => s"-${joinV(l)}-${joinV(r)}" }
          (Compile / unmanagedSourceDirectories).value.flatMap {
            case dir if dir.getPath.endsWith("scala") => rangeVersions.map { vStr => file(dir.getPath + vStr) }
            case _ => Seq.empty
          }
      }
    },
    Test / unmanagedSourceDirectories ++= {
      val crossVersions = crossScalaVersions.value
      import Ordering.Implicits._
      val ltEqVersions = crossVersions.map(CrossVersion.partialVersion).sorted.flatten
      def joinV = (_: Product).productIterator.mkString(".")
      val allRangeVersions = (2 to math.max(2, ltEqVersions.size - 1))
        .flatMap(i => ltEqVersions.sliding(i).filter(_.size == i))
        .map(l => (l.head, l.last))
      CrossVersion.partialVersion(scalaVersion.value).toList.flatMap {
        version =>
          val rangeVersions = allRangeVersions
            .filter { case (l, r) => l <= version && version <= r }
            .map { case (l, r) => s"-${joinV(l)}-${joinV(r)}" }
          (Test / unmanagedSourceDirectories).value.flatMap {
            case dir if dir.getPath.endsWith("scala") => rangeVersions.map { vStr => file(dir.getPath + vStr) }
            case _ => Seq.empty
          }
      }
    },
    Compile / doc / sources := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "3.1.0") => Seq(
      
      )
      case (_, _) => (Compile / doc / sources).value
    } },
    Test / testOptions += Tests.Argument("-oDF"),
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.11.12") => Seq.empty
      case (_, "2.12.15") => Seq(
        "-target:jvm-1.8",
        "-explaintypes",
        "-Ypartial-unification",
        if (insideCI.value) "-Wconf:any:error" else "-Wconf:any:warning",
        "-Wconf:cat=optimizer:warning",
        "-Wconf:cat=other-match-analysis:error",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
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
        "-Ycache-macro-class-loader:last-modified",
        "-Wconf:msg=nowarn:silent"
      )
      case (_, "2.13.7") => Seq(
        "-target:jvm-1.8",
        "-explaintypes",
        if (insideCI.value) "-Wconf:any:error" else "-Wconf:any:warning",
        "-Wconf:cat=optimizer:warning",
        "-Wconf:cat=other-match-analysis:error",
        "-Vtype-diffs",
        "-Ybackend-parallelism",
        math.min(16, math.max(1, sys.runtime.availableProcessors() - 1)).toString,
        "-Wdead-code",
        "-Wextra-implicit",
        "-Wnumeric-widen",
        "-Woctal-literal",
        "-Wvalue-discard",
        "-Wunused:_",
        "-Wmacros:after",
        "-Ycache-plugin-class-loader:always",
        "-Ycache-macro-class-loader:last-modified",
        "-Wconf:msg=nowarn:silent"
      )
      case (_, _) => Seq(
        "-Ykind-projector",
        "-no-indent",
        "-language:implicitConversions"
      )
    } },
    scalacOptions -= "-Wconf:any:error",
    mimaPreviousArtifacts := { (isSnapshot.value, scalaVersion.value) match {
      case (_, "3.1.0") => Set.empty
      case (_, _) => Set(organization.value %% name.value % "1.0.0")
    } },
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (_, "2.13.7") => Seq(
        "-Xlint:-implicit-recursion"
      )
      case (_, _) => Seq.empty
    } },
    scalacOptions ++= { (isSnapshot.value, scalaVersion.value) match {
      case (false, "2.12.15") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (false, "2.13.7") => Seq(
        "-opt:l:inline",
        "-opt-inline-from:izumi.reflect.**"
      )
      case (_, _) => Seq.empty
    } }
  )

lazy val `izumi-reflect-aggregate` = (project in file(".agg/izumi-reflect-izumi-reflect-aggregate"))
  .settings(
    publish / skip := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shaded`,
    `izumi-reflect`
  )

lazy val `izumi-reflect-aggregate-jvm` = (project in file(".agg/izumi-reflect-izumi-reflect-aggregate-jvm"))
  .settings(
    publish / skip := true,
    crossScalaVersions := Nil
  )
  .aggregate(
    `izumi-reflect-thirdparty-boopickle-shaded`,
    `izumi-reflect`
  )

lazy val `izumi-reflect-root-jvm` = (project in file(".agg/.agg-jvm"))
  .settings(
    publish / skip := true,
    crossScalaVersions := Seq(
      "3.1.0",
      "2.13.7",
      "2.12.15",
      "2.11.12"
    ),
    scalaVersion := crossScalaVersions.value.head
  )
  .aggregate(
    `izumi-reflect-aggregate-jvm`
  )

lazy val `izumi-reflect-root` = (project in file("."))
  .settings(
    publish / skip := true,
    Global / onChangedBuildSource := ReloadOnSourceChanges,
    ThisBuild / publishMavenStyle := true,
    ThisBuild / scalacOptions ++= Seq(
      "-encoding",
      "UTF-8",
      "-feature",
      "-unchecked",
      "-deprecation",
      "-language:higherKinds"
    ),
    ThisBuild / javacOptions ++= Seq(
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
    crossScalaVersions := Nil,
    scalaVersion := "3.1.0",
    ThisBuild / organization := "dev.zio",
    sonatypeProfileName := "dev.zio",
    sonatypeSessionName := s"[sbt-sonatype] ${name.value} ${version.value} ${java.util.UUID.randomUUID}",
    ThisBuild / publishTo := 
    (if (!isSnapshot.value) {
        sonatypePublishToBundle.value
      } else {
        Some(Opts.resolver.sonatypeSnapshots)
    })
    ,
    ThisBuild / credentials += Credentials(file(".secrets/credentials.sonatype-nexus.properties")),
    ThisBuild / homepage := Some(url("https://zio.dev")),
    ThisBuild / licenses := Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    ThisBuild / developers := List(
              Developer(id = "jdegoes", name = "John De Goes", url = url("http://degoes.net"), email = "john@degoes.net"),
              Developer(id = "7mind", name = "Septimal Mind", url = url("https://github.com/7mind"), email = "team@7mind.io"),
            ),
    ThisBuild / scmInfo := Some(ScmInfo(url("https://github.com/zio/izumi-reflect"), "scm:git:https://github.com/zio/izumi-reflect.git")),
    ThisBuild / mimaBinaryIssueFilters ++= Seq(
      ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTag.binaryFormatVersion"),
      ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef.repr"),
      ProblemFilters.exclude[Problem]("izumi.reflect.TagMacro.*"),
      ProblemFilters.exclude[Problem]("izumi.reflect.macrortti.LightTypeTagImpl.*"),
      ProblemFilters.exclude[Problem]("izumi.reflect.macrortti.LightTypeTagImpl#*"),
      ProblemFilters.exclude[Problem]("izumi.reflect.thirdparty.*"),
      ProblemFilters.exclude[Problem]("izumi.reflect.internal.*"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagImpl.norm"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagImpl.izumi$reflect$macrortti$LightTypeTagImpl$$*"),
      ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagInheritance.CtxExt"),
      ProblemFilters.exclude[MissingTypesProblem]       ("izumi.reflect.macrortti.LightTypeTagInheritance$Ctx*"),
      ProblemFilters.exclude[Problem]                   ("izumi.reflect.macrortti.LightTypeTagInheritance#Ctx*"),
      ProblemFilters.exclude[Problem]                   ("izumi.reflect.macrortti.LightTypeTagUnpacker*")
    ),
    ThisBuild / mimaFailOnProblem := true,
    ThisBuild / mimaFailOnNoPrevious := false,
    libraryDependencies += "io.7mind.izumi.sbt" % "sbtgen_2.13" % "0.0.89" % Provided
  )
  .aggregate(
    `izumi-reflect-aggregate`
  )
