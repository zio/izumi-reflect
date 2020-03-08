import $ivy.`io.7mind.izumi.sbt::sbtgen:0.0.50`
import izumi.sbtgen._
import izumi.sbtgen.model._

object Izumi {

  object V {
    val silencer = Version.VExpr("V.silencer")
    val kind_projector = Version.VExpr("V.kind_projector")
    val scalatest = Version.VExpr("V.scalatest")
  }

  object PV {
    val sbt_mdoc = Version.VExpr("PV.sbt_mdoc")
    val sbt_paradox_material_theme = Version.VExpr("PV.sbt_paradox_material_theme")
    val sbt_ghpages = Version.VExpr("PV.sbt_ghpages")
    val sbt_site = Version.VExpr("PV.sbt_site")
    val sbt_unidoc = Version.VExpr("PV.sbt_unidoc")
    val sbt_scoverage = Version.VExpr("PV.sbt_scoverage")
    val sbt_pgp = Version.VExpr("PV.sbt_pgp")

    val scala_js_version = Version.VExpr("PV.scala_js_version")
    val scala_native_version = Version.VExpr("PV.scala_native_version")
    val crossproject_version = Version.VExpr("PV.crossproject_version")
    val scalajs_bundler_version = Version.VExpr("PV.scalajs_bundler_version")
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
    bundlerVersion = PV.scalajs_bundler_version,
  )

  object Deps {
    final val scalatest = Library("org.scalatest", "scalatest", V.scalatest, LibraryType.Auto) in Scope.Test.all

    final val scala_reflect = Library("org.scala-lang", "scala-reflect", Version.VExpr("scalaVersion.value"), LibraryType.Invariant)

    final val projector = Library("org.typelevel", "kind-projector", V.kind_projector, LibraryType.Invariant)
      .more(LibSetting.Raw("cross CrossVersion.full"))
    final val silencer_plugin = Library("com.github.ghik", "silencer-plugin", V.silencer, LibraryType.Invariant)
      .more(LibSetting.Raw("cross CrossVersion.full"))
    final val silencer_lib = Library("com.github.ghik", "silencer-lib", V.silencer, LibraryType.Invariant)
      .more(LibSetting.Raw("cross CrossVersion.full"))
  }

  import Deps._

  // DON'T REMOVE, these variables are read from CI build (build.sh)
  final val scala211 = ScalaVersion("2.11.12")
  final val scala212 = ScalaVersion("2.12.10")
  final val scala213 = ScalaVersion("2.13.1")

  object Groups {
    final val fundamentals = Set(Group("fundamentals"))
  }

  object Targets {
    // switch order to use 2.13 in IDEA
    val targetScala = Seq(scala212, scala213)
    val targetScalaN = Seq(scala212, scala213, scala211)
    //    val targetScala = Seq(scala213, scala212)
    private val jvmPlatform = PlatformEnv(
      platform = Platform.Jvm,
      language = targetScala,
    )
    private val jvmPlatformNative = PlatformEnv(
      platform = Platform.Jvm,
      language = targetScalaN,
    )
    private val jsPlatform = PlatformEnv(
      platform = Platform.Js,
      language = targetScala,
      settings = Seq(
        "coverageEnabled" := false,
        "scalaJSModuleKind" in (SettingScope.Project, Platform.Js) := "ModuleKind.CommonJSModule".raw,
      ),
    )
    private val jsPlatformN = PlatformEnv(
      platform = Platform.Js,
      language = targetScalaN,
      settings = Seq(
        "coverageEnabled" := false,
        "scalaJSModuleKind" in (SettingScope.Project, Platform.Js) := "ModuleKind.CommonJSModule".raw,
      ),
    )
    private val nativePlatform = PlatformEnv(
      platform = Platform.Native,
      language = Seq(scala211),
      settings = Seq(
        "coverageEnabled" := false,
      ),
    )
    private val jvmPlatformSbt = PlatformEnv(
      platform = Platform.Jvm,
      language = Seq(scala212),
      settings = Seq(
        "coverageEnabled" := false,
      ),
    )
    final val cross = Seq(jvmPlatform, jsPlatform)
    final val crossNative = Seq(jvmPlatformNative, jsPlatformN, nativePlatform)
    final val jvm = Seq(jvmPlatform)
    final val js = Seq(jsPlatform)
    final val jvmSbt = Seq(jvmPlatformSbt)
  }

  object Projects {

    final val plugins = Plugins(
      Seq.empty,
      Seq.empty,
    )

    object root {
      final val id = ArtifactId("izumi-reflect-root")
      final val plugins = Plugins(
        enabled = Seq(Plugin("SbtgenVerificationPlugin")),
        disabled = Seq.empty,
      )
      final val settings = Seq()

      final val sharedAggSettings = Seq(
        "crossScalaVersions" := Targets.targetScala.map(_.value),
        "scalaVersion" := "crossScalaVersions.value.head".raw,
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
          "licenses" in SettingScope.Build := """Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))""".raw,
          "developers" in SettingScope.Build :=
          """List(
          Developer(id = "jdegoes", name = "John De Goes", url = url("http://degoes.net"), email = "john@degoes.net"),
          Developer(id = "7mind", name = "Septimal Mind", url = url("https://github.com/7mind"), email = "team@7mind.io"),
        )""".raw,
          "scmInfo" in SettingScope.Build := """Some(ScmInfo(url("https://github.com/zio/izumi-reflect"), "scm:git:https://github.com/zio/izumi-reflect.git"))""".raw,
          "scalacOptions" in SettingScope.Build += s"""${"\"" * 3}-Xmacro-settings:scalatest-version=${V.scalatest}${"\"" * 3}""".raw,
        )

      final val sharedSettings = Defaults.SbtMeta ++ Seq(
          "test" in Platform.Native := "{}".raw,
          "test" in (SettingScope.Test, Platform.Native) := "{}".raw,
          "testOptions" in SettingScope.Test += """Tests.Argument("-oDF")""".raw,
          //"testOptions" in (SettingScope.Test, Platform.Jvm) ++= s"""Seq(Tests.Argument("-u"), Tests.Argument(s"$${target.value}/junit-xml-$${scalaVersion.value}"))""".raw,
          "scalacOptions" ++= Seq(
            SettingKey(Some(scala212), None) := Defaults.Scala212Options,
            SettingKey(Some(scala213), None) := Defaults.Scala213Options,
            SettingKey.Default := Const.EmptySeq,
          ),
          "scalacOptions" ++= Seq(
            SettingKey(Some(scala212), Some(true)) := Seq(
              "-opt:l:inline",
              "-opt-inline-from:izreflect.**",
            ),
            SettingKey(Some(scala213), Some(true)) := Seq(
              "-opt:l:inline",
              "-opt-inline-from:izreflect.**",
            ),
            SettingKey.Default := Const.EmptySeq
          ),
        )

    }

    object fundamentals {
      final val id = ArtifactId("fundamentals")
      final val basePath = Seq("fundamentals")

      final val reflection = ArtifactId("izumi-reflect")

      final val thirdpartyBoopickleShaded = ArtifactId("izumi-reflect-thirdparty-boopickle-shaded")
    }

  }

  final val crossScalaSources = Seq(
    "unmanagedSourceDirectories" in SettingScope.Compile :=
    """(unmanagedSourceDirectories in Compile).value.flatMap {
      |  dir =>
      |   Seq(dir, file(dir.getPath + (CrossVersion.partialVersion(scalaVersion.value) match {
      |     case Some((2, 11)) => "_2.11"
      |     case Some((2, 12)) => "_2.12"
      |     case Some((2, 13)) => "_2.13"
      |     case _             => "_3.0"
      |   })))
      |}""".stripMargin.raw,
  )

  final lazy val fundamentals = Aggregate(
    name = Projects.fundamentals.id,
    artifacts = Seq(
      Artifact(
        name = Projects.fundamentals.thirdpartyBoopickleShaded,
        libs = Seq(
          scala_reflect in Scope.Provided.all
        ),
        depends = Seq.empty,
        settings = crossScalaSources ++ Seq(
            SettingDef.RawSettingDef(
              """scalacOptions in Compile --= Seq("-Ywarn-value-discard","-Ywarn-unused:_", "-Wvalue-discard", "-Wunused:_")""",
              FullSettingScope(SettingScope.Compile, Platform.All),
            ),
          ),
      ),
      Artifact(
        name = Projects.fundamentals.reflection,
        libs = Seq(scala_reflect in Scope.Provided.all),
        settings = crossScalaSources,
        depends = Seq(
          Projects.fundamentals.thirdpartyBoopickleShaded,
        ),
      ),
    ),
    pathPrefix = Projects.fundamentals.basePath,
    groups = Groups.fundamentals,
    defaultPlatforms = Targets.crossNative,
    enableProjectSharedAggSettings = false,
    settings = Seq(
      "crossScalaVersions" := "Nil".raw,
    )
  )

  val izumi_reflect: Project = Project(
    name = Projects.root.id,
    aggregates = Seq(
      fundamentals,
    ),
    topLevelSettings = Projects.root.settings,
    sharedSettings = Projects.root.sharedSettings,
    sharedAggSettings = Projects.root.sharedAggSettings,
    rootSettings = Projects.root.rootSettings,
    imports = Seq.empty,
    globalLibs = Seq(
      ScopedLibrary(projector, FullDependencyScope(Scope.Compile, Platform.All), compilerPlugin = true),
      ScopedLibrary(silencer_plugin, FullDependencyScope(Scope.Compile, Platform.All), compilerPlugin = true),
      silencer_lib in Scope.Provided.all,
      scalatest,
    ),
    rootPlugins = Projects.root.plugins,
    globalPlugins = Projects.plugins,
    pluginConflictRules = Map.empty,
    appendPlugins = Defaults.SbtGenPlugins ++ Seq(
        SbtPlugin("com.jsuereth", "sbt-pgp", PV.sbt_pgp),
        SbtPlugin("org.scoverage", "sbt-scoverage", PV.sbt_scoverage),
        SbtPlugin("com.eed3si9n", "sbt-unidoc", PV.sbt_unidoc),
        SbtPlugin("com.typesafe.sbt", "sbt-site", PV.sbt_site),
        SbtPlugin("com.typesafe.sbt", "sbt-ghpages", PV.sbt_ghpages),
        SbtPlugin("io.github.jonas", "sbt-paradox-material-theme", PV.sbt_paradox_material_theme),
        SbtPlugin("org.scalameta", "sbt-mdoc", PV.sbt_mdoc),
      )
  )
}
