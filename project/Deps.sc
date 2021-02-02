import $ivy.`io.7mind.izumi.sbt::sbtgen:0.0.67`
import izumi.sbtgen._
import izumi.sbtgen.model._

object Izumi {

  object V {
    val silencer = Version.VExpr("V.silencer")
    val collection_compat = Version.VExpr("V.collection_compat")
    val kind_projector = Version.VExpr("V.kind_projector")
    val scalatest = Version.VExpr("V.scalatest")
  }

  object PV {
    val sbt_scoverage = Version.VExpr("PV.sbt_scoverage")
    val sbt_pgp = Version.VExpr("PV.sbt_pgp")

    val scala_js_version = Version.VExpr("PV.scala_js_version")
    val scala_native_version = Version.VExpr("PV.scala_native_version")
    val crossproject_version = Version.VExpr("PV.crossproject_version")
    val scalajs_bundler_version = Version.VExpr("PV.scalajs_bundler_version")
    val sbt_dotty_version = Version.VExpr("PV.sbt_dotty_version")
    val sbt_mima_version = Version.VExpr("PV.sbt_mima_version")
  }

  def entrypoint(args: Seq[String]) = {
    Entrypoint.main(izumi_reflect, settings, Seq("-o", ".") ++ args)
  }

  val settings = GlobalSettings(
    groupId = "dev.zio",
    sbtVersion = None,
    scalaJsVersion = PV.scala_js_version,
    scalaNativeVersion = PV.scala_native_version,
    crossProjectVersion = PV.crossproject_version,
    bundlerVersion = Some(PV.scalajs_bundler_version),
    sbtDottyVersion = PV.sbt_dotty_version
  )

  object Deps {
    final val scalatest = Library("org.scalatest", "scalatest", V.scalatest, LibraryType.Auto)
    // thanks, Sandinh!
//    final val scalatest_dotty = Library("com.sandinh", "scalatest", V.scalatest, LibraryType.Auto)
    final val scalatest_dotty = scalatest

    final val scala_reflect = Library("org.scala-lang", "scala-reflect", Version.VExpr("scalaVersion.value"), LibraryType.Invariant)

    final val projector = Library("org.typelevel", "kind-projector", V.kind_projector, LibraryType.Invariant)
      .more(LibSetting.Raw("cross CrossVersion.full"))
    final val silencer_plugin = Library("com.github.ghik", "silencer-plugin", V.silencer, LibraryType.Invariant)
      .more(LibSetting.Raw("cross CrossVersion.full"))
    final val collection_compat = Library("org.scala-lang.modules", "scala-collection-compat", V.collection_compat, LibraryType.Auto)
  }

  import Deps._

  // DON'T REMOVE, these variables are read from CI build (build.sh)
  final val scala211 = ScalaVersion("2.11.12")
  final val scala212 = ScalaVersion("2.12.13")
  final val scala213 = ScalaVersion("2.13.4")
  final val scala3 = ScalaVersion("3.0.0-M3")

  object Groups {
    final val izumi_reflect = Set(Group("izumi-reflect"))
  }

  object Targets {
    // switch order to use dotty in IDEA
//    val targetScala = Seq(scala211, scala212, scala213, scala3)
//    val targetScala = Seq(scala212, scala213, scala211, scala3)
//    val targetScala = Seq(scala213, scala212, scala211, scala3)
    val targetScala = Seq(scala3, scala213, scala212, scala211)
    private val jvmPlatform = PlatformEnv(
      platform = Platform.Jvm,
      language = targetScala
    )
    private val jsPlatform = PlatformEnv(
      platform = Platform.Js,
      language = targetScala.filterNot(_.isDotty),
      settings = Seq(
        "coverageEnabled" := false,
        "scalaJSLinkerConfig" in (SettingScope.Project, Platform.Js) := "scalaJSLinkerConfig.value.withModuleKind(ModuleKind.CommonJSModule)".raw
      )
    )
    private val nativePlatform = PlatformEnv(
      platform = Platform.Native,
      language = targetScala.filterNot(_.isDotty),
      settings = Seq(
        "coverageEnabled" := false
      )
    )
    final val crossNative = Seq(jvmPlatform, jsPlatform, nativePlatform)
  }

  object Projects {

    object root {
      final val id = ArtifactId("izumi-reflect-root")
      final val plugins = Plugins(
        enabled = Seq(Plugin("SbtgenVerificationPlugin")),
        disabled = Seq.empty
      )
      final val settings = Seq()

      final val sharedAggSettings = Seq(
        "crossScalaVersions" := Targets.targetScala.map(_.value),
        "scalaVersion" := "crossScalaVersions.value.head".raw
      )

      final val rootSettings = Defaults.SharedOptions ++ Seq(
        "crossScalaVersions" := "Nil".raw,
        "scalaVersion" := Targets.targetScala.head.value,
        "organization" in SettingScope.Build := "dev.zio",
        "sonatypeProfileName" := "dev.zio",
        "sonatypeSessionName" := """s"[sbt-sonatype] ${name.value} ${version.value} ${java.util.UUID.randomUUID}"""".raw,
        "publishTo" in SettingScope.Build :=
          """
            |(if (!isSnapshot.value) {
            |    sonatypePublishToBundle.value
            |  } else {
            |    Some(Opts.resolver.sonatypeSnapshots)
            |})
            |""".stripMargin.raw,
        "credentials" in SettingScope.Build += """Credentials(file(".secrets/credentials.sonatype-nexus.properties"))""".raw,
        "homepage" in SettingScope.Build := """Some(url("https://zio.dev"))""".raw,
        "licenses" in SettingScope.Build := """Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))""".raw,
        "developers" in SettingScope.Build :=
          """List(
          Developer(id = "jdegoes", name = "John De Goes", url = url("http://degoes.net"), email = "john@degoes.net"),
          Developer(id = "7mind", name = "Septimal Mind", url = url("https://github.com/7mind"), email = "team@7mind.io"),
        )""".raw,
        "scmInfo" in SettingScope.Build := """Some(ScmInfo(url("https://github.com/zio/izumi-reflect"), "scm:git:https://github.com/zio/izumi-reflect.git"))""".raw,
        "scalacOptions" in SettingScope.Build += s"""${"\"" * 3}-Xmacro-settings:scalatest-version=${V.scalatest}${"\"" * 3}""".raw,
        "mimaBinaryIssueFilters" in SettingScope.Build ++= Seq(
          """ProblemFilters.exclude[Problem]("izumi.reflect.TagMacro.*")""".raw,
          """ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.Tag.refinedTag")""".raw,
          """ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTag.refinedType")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef#RefinementDecl.name")""".raw
        ),
        "mimaFailOnProblem" in SettingScope.Build := true,
        "mimaFailOnNoPrevious" in SettingScope.Build := false
      )

      final val sharedSettings = Defaults.SbtMetaOptions ++ Seq(
        "test" in Platform.Native := "{}".raw,
        "test" in (SettingScope.Test, Platform.Native) := "{}".raw,
        "sources" in SettingScope.Raw("(Compile, doc)") := Seq(
          SettingKey(Some(scala3), None) := Seq.empty[String],
          SettingKey.Default := "(sources in (Compile, doc)).value".raw
        ),
        "testOptions" in SettingScope.Test += """Tests.Argument("-oDF")""".raw,
        //"testOptions" in (SettingScope.Test, Platform.Jvm) ++= s"""Seq(Tests.Argument("-u"), Tests.Argument(s"$${target.value}/junit-xml-$${scalaVersion.value}"))""".raw,
        "scalacOptions" ++= Seq(
          SettingKey(Some(scala212), None) := Defaults.Scala212Options,
          SettingKey(Some(scala213), None) := Defaults.Scala213Options,
          SettingKey(Some(scala211), None) := Const.EmptySeq,
          SettingKey.Default := Seq(
            "-Ykind-projector",
            "-noindent",
            "-language:implicitConversions"
          )
        ),
        "mimaPreviousArtifacts" := Seq(
          SettingKey(Some(scala3), None) := "Set.empty".raw,
          SettingKey.Default := """Set(organization.value %% name.value % "1.0.0-M2")""".raw
        ),
        "scalacOptions" ++= Seq(
          SettingKey(Some(scala212), None) := Seq(
            "-Wconf:msg=nowarn:silent"
          ),
          SettingKey(Some(scala213), None) := Seq(
            "-Xlint:-implicit-recursion"
          ),
          SettingKey.Default := Const.EmptySeq
        ),
        "scalacOptions" -= "-Wconf:any:error",
        "scalacOptions" ++= Seq(
          SettingKey(Some(scala212), Some(true)) := Seq(
            "-opt:l:inline",
            "-opt-inline-from:izumi.reflect.**"
          ),
          SettingKey(Some(scala213), Some(true)) := Seq(
            "-opt:l:inline",
            "-opt-inline-from:izumi.reflect.**"
          ),
          SettingKey.Default := Const.EmptySeq
        )
      )

    }

    object izumi_reflect_aggregate {
      final val id = ArtifactId("izumi-reflect-aggregate")
      final val basePath = Seq("izumi-reflect")

      final val izumi_reflect = ArtifactId("izumi-reflect")
      final val thirdpartyBoopickleShaded = ArtifactId("izumi-reflect-thirdparty-boopickle-shaded")
    }

  }

  final lazy val izumi_reflect_aggregate = Aggregate(
    name = Projects.izumi_reflect_aggregate.id,
    artifacts = Seq(
      Artifact(
        name = Projects.izumi_reflect_aggregate.thirdpartyBoopickleShaded,
        libs = Seq.empty,
        depends = Seq.empty,
        settings = Defaults.CrossScalaSources ++ Seq(
          SettingDef.RawSettingDef(
            """scalacOptions in Compile --= Seq("-Ywarn-value-discard","-Ywarn-unused:_", "-Wvalue-discard", "-Wunused:_")""",
            FullSettingScope(SettingScope.Compile, Platform.All)
          )
        )
      ),
      Artifact(
        name = Projects.izumi_reflect_aggregate.izumi_reflect,
        libs = Seq.empty,
        settings = Defaults.CrossScalaSources,
        depends = Seq(
          Projects.izumi_reflect_aggregate.thirdpartyBoopickleShaded
        )
      )
    ),
    pathPrefix = Projects.izumi_reflect_aggregate.basePath,
    groups = Groups.izumi_reflect,
    defaultPlatforms = Targets.crossNative,
    enableProjectSharedAggSettings = false,
    settings = Seq(
      "crossScalaVersions" := "Nil".raw
    )
  )

  val izumi_reflect: Project = Project(
    name = Projects.root.id,
    aggregates = Seq(
      izumi_reflect_aggregate
    ),
    topLevelSettings = Projects.root.settings,
    sharedSettings = Projects.root.sharedSettings,
    sharedAggSettings = Projects.root.sharedAggSettings,
    rootSettings = Projects.root.rootSettings,
    imports = Seq(
      Import("com.typesafe.tools.mima.core._")
    ),
    globalLibs = Seq(
      ScopedLibrary(projector, FullDependencyScope(Scope.Compile, Platform.All).scalaVersion(ScalaVersionScope.AllScala2), compilerPlugin = true),
      ScopedLibrary(silencer_plugin, FullDependencyScope(Scope.Compile, Platform.All).scalaVersion(ScalaVersionScope.AllScala2), compilerPlugin = true),
      collection_compat in Scope.Provided.all.scalaVersion(ScalaVersionScope.AllScala2),
      scala_reflect in Scope.Provided.all.scalaVersion(ScalaVersionScope.AllScala2),
      scalatest in Scope.Test.all
    ),
    rootPlugins = Projects.root.plugins,
    globalPlugins = Plugins(),
    pluginConflictRules = Map.empty,
    appendPlugins = Defaults.SbtGenPlugins ++ Seq(
      SbtPlugin("com.jsuereth", "sbt-pgp", PV.sbt_pgp),
      SbtPlugin("org.scoverage", "sbt-scoverage", PV.sbt_scoverage),
      SbtPlugin("com.typesafe", "sbt-mima-plugin", PV.sbt_mima_version)
    )
  )
}
