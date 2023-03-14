import $ivy.`io.7mind.izumi.sbt:sbtgen_2.13:0.0.97`
import izumi.sbtgen._
import izumi.sbtgen.model._

object Izumi {

  object V {
    val collection_compat = Version.VExpr("V.collection_compat")
    val kind_projector = Version.VExpr("V.kind_projector")
    val scalatest = Version.VExpr("V.scalatest")
    val sbtgen = Version.VExpr("V.sbtgen")
  }

  object PV {
    val sbt_scoverage = Version.VExpr("PV.sbt_scoverage")
    val sbt_pgp = Version.VExpr("PV.sbt_pgp")

    val scala_js_version = Version.VExpr("PV.scala_js_version")
    val scala_native_version = Version.VExpr("PV.scala_native_version")
    val crossproject_version = Version.VExpr("PV.crossproject_version")
    val scalajs_bundler_version = Version.VExpr("PV.scalajs_bundler_version")
    val sbt_mima_version = Version.VExpr("PV.sbt_mima_version")
  }

  // DON'T REMOVE, these variables are read from CI build (build.sh)
  final val scala211 = ScalaVersion("2.11.12")
  final val scala212 = ScalaVersion("2.12.17")
  final val scala213 = ScalaVersion("2.13.10")
  final val scala300 = ScalaVersion("3.2.2")

  // launch with `./sbtgen.sc 2` to use 2.13 in Intellij
  var targetScala = Seq(scala300, scala213, scala212, scala211)

  def entrypoint(args: Seq[String]) = {
    val newArgs = args diff Seq(
      args
        .collectFirst {
          case s @ s"3${_}" => s -> scala300
          case s @ "2.12" => s -> scala212
          case s @ "2.11" => s -> scala211
          case s @ s"2${_}" => s -> scala213
        }.map {
          case (s, target) =>
            targetScala = target :: targetScala.filterNot(_ == target).toList
            s
        }.orNull
    )
    if (args.contains("--help")) {
      println(
        "launch with `./sbtgen.sc 3` to use Scala 3 in IDEA, launch with `./sbtgen.sc 2` to use Scala 2"
      )
    }
    println(s"Scala targets: $targetScala")
    Entrypoint.main(izumi_reflect, settings, Seq("-o", ".") ++ newArgs)
  }

  val settings = GlobalSettings(
    groupId = "dev.zio",
    sbtVersion = None,
    scalaJsVersion = Version.VExpr("PV.scala_js_version"),
    scalaNativeVersion = Version.VExpr("PV.scala_native_version"),
    crossProjectVersion = Version.VExpr("PV.sbt_crossproject_version")
  )

  object Deps {
    final val scalatest = Library("org.scalatest", "scalatest", V.scalatest, LibraryType.Auto)

    final val scala_reflect = Library("org.scala-lang", "scala-reflect", Version.VExpr("scalaVersion.value"), LibraryType.Invariant)
    final val scala3_compiler = Library("org.scala-lang", "scala3-compiler", Version.VExpr("scalaVersion.value"), LibraryType.AutoJvm)

    final val projector = Library("org.typelevel", "kind-projector", V.kind_projector, LibraryType.Invariant)
      .more(LibSetting.Raw("cross CrossVersion.full"))

    final val collection_compat = Library("org.scala-lang.modules", "scala-collection-compat", V.collection_compat, LibraryType.Auto)
  }

  import Deps._

  object Groups {
    final val izumi_reflect = Set(Group("izumi-reflect"))
  }

  object Targets {
    def targetScala = Izumi.targetScala
    private val jvmPlatform = PlatformEnv(
      platform = Platform.Jvm,
      language = targetScala
    )
    private val jsPlatform = PlatformEnv(
      platform = Platform.Js,
      language = targetScala,
      settings = Seq(
        "coverageEnabled" := false,
        "scalaJSLinkerConfig" in (SettingScope.Project, Platform.Js) := "scalaJSLinkerConfig.value.withModuleKind(ModuleKind.CommonJSModule)".raw
      )
    )
    private val nativePlatform = PlatformEnv(
      platform = Platform.Native,
      language = targetScala,
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

      final val rootSettings = Defaults.RootOptions ++ Seq(
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
        "credentials" in SettingScope.Build ++=
          """
            |{
            |val credTarget = Path.userHome / ".sbt" / "secrets" / "credentials.sonatype-nexus.properties"
            |if (credTarget.exists) {
            |  Seq(Credentials(credTarget))
            |} else {
            |  Seq.empty
            |}
            |}""".stripMargin.raw,
        "credentials" in SettingScope.Build ++=
          """
            |{
            |val credTarget = file(".") / ".secrets" / "credentials.sonatype-nexus.properties"
            |if (credTarget.exists) {
            |  Seq(Credentials(credTarget))
            |} else {
            |  Seq.empty
            |}
            |}""".stripMargin.raw,
        "homepage" in SettingScope.Build := """Some(url("https://zio.dev"))""".raw,
        "licenses" in SettingScope.Build := """Seq("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0"))""".raw,
        "developers" in SettingScope.Build :=
          """List(
          Developer(id = "jdegoes", name = "John De Goes", url = url("http://degoes.net"), email = "john@degoes.net"),
          Developer(id = "7mind", name = "Septimal Mind", url = url("https://github.com/7mind"), email = "team@7mind.io"),
        )""".raw,
        "scmInfo" in SettingScope.Build := """Some(ScmInfo(url("https://github.com/zio/izumi-reflect"), "scm:git:https://github.com/zio/izumi-reflect.git"))""".raw,
        "mimaBinaryIssueFilters" in SettingScope.Build ++= Seq(
          // ignore deletion of old ParsedLightTypeTag* classes, they were made package private more than a year ago, they shouldn't appear in library sources anymore - or in the first place
          """ProblemFilters.exclude[MissingClassProblem]("izumi.reflect.macrortti.LightTypeTag$ParsedLightTypeTag")""".raw,
          """ProblemFilters.exclude[MissingClassProblem]("izumi.reflect.macrortti.LightTypeTag$ParsedLightTypeTag110")""".raw,
          """ProblemFilters.exclude[MissingClassProblem]("izumi.reflect.macrortti.LightTypeTag$ParsedLightTypeTag210")""".raw,
          """ProblemFilters.exclude[MissingClassProblem]("izumi.reflect.macrortti.LightTypeTag$ParsedLightTypeTagM8")""".raw,
          // dotty-only ProductX case class inheritance breakage
          """ProblemFilters.exclude[IncompatibleResultTypeProblem]("izumi.reflect.macrortti.LightTypeTagRef#FullReference._1")""".raw,
          // new inherited methods added (2.11 problem only?)
          """ProblemFilters.exclude[InheritedNewAbstractMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef*")""".raw,
          // new methods added
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LTTRenderables.r_LambdaParameterName")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTag.binaryFormatVersion")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef.repr")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LTTRenderables.r_Wildcard")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LTTRenderables.prefixSplitter")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef.longNameWithPrefix")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef.longNameInternalSymbol")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef#AppliedNamedReference.symName")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef#AppliedNamedReference.prefix")""".raw,
          """ProblemFilters.exclude[ReversedMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagRef.scalaStyledName")""".raw,
          // compile-time only
          """ProblemFilters.exclude[Problem]("izumi.reflect.TagMacro.*")""".raw,
          """ProblemFilters.exclude[Problem]("izumi.reflect.macrortti.LightTypeTagImpl.*")""".raw,
          """ProblemFilters.exclude[Problem]("izumi.reflect.macrortti.LightTypeTagImpl#*")""".raw,
          """ProblemFilters.exclude[Problem]("izumi.reflect.dottyreflection.*")""".raw,
          // private packages
          """ProblemFilters.exclude[Problem]("izumi.reflect.thirdparty.*")""".raw,
          """ProblemFilters.exclude[Problem]("izumi.reflect.internal.*")""".raw,
          // private methods
          """ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagImpl.norm")""".raw,
          """ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagImpl.izumi$reflect$macrortti$LightTypeTagImpl$$*")""".raw,
          // private types
          """ProblemFilters.exclude[DirectMissingMethodProblem]("izumi.reflect.macrortti.LightTypeTagInheritance.CtxExt")""".raw,
          """ProblemFilters.exclude[MissingTypesProblem]       ("izumi.reflect.macrortti.LightTypeTagInheritance$Ctx*")""".raw,
          """ProblemFilters.exclude[Problem]                   ("izumi.reflect.macrortti.LightTypeTagInheritance#Ctx*")""".raw,
          """ProblemFilters.exclude[Problem]                   ("izumi.reflect.macrortti.LightTypeTagUnpacker*")""".raw
        ),
        "mimaFailOnProblem" in SettingScope.Build := true,
        "mimaFailOnNoPrevious" in SettingScope.Build := false,

        // scala-steward workaround
        // add sbtgen version to sbt build to allow scala-steward to find it and update it in .sc files
        // https://github.com/scala-steward-org/scala-steward/issues/696#issuecomment-545800968
        "libraryDependencies" += s""""io.7mind.izumi.sbt" % "sbtgen_2.13" % "${Version.SbtGen.value}" % Provided""".raw
      )

      final val sharedSettings =
        Defaults.CrossScalaPlusSources ++
        Defaults.CrossScalaRangeSources ++
        Seq(
          "test" in Platform.Native := "{}".raw,
          "test" in (SettingScope.Test, Platform.Native) := "{}".raw,
          "sources" in SettingScope.Raw("Compile / doc") := Seq(
            SettingKey(Some(scala300), None) := Seq.empty[String],
            SettingKey.Default := "(Compile / doc / sources).value".raw
          ),
          "testOptions" in SettingScope.Test += """Tests.Argument("-oDF")""".raw,
          // "testOptions" in (SettingScope.Test, Platform.Jvm) ++= s"""Seq(Tests.Argument("-u"), Tests.Argument(s"$${target.value}/junit-xml-$${scalaVersion.value}"))""".raw,
          "scalacOptions" ++= {
            val removedOpts = Set[Const](
              "-P:kind-projector:underscore-placeholders",
              "-Vimplicits",
              "-Xsource:3" // FIXME return after dropping 2.11
            )
            val addedOpts = Seq[Const](
              "-Wconf:msg=nowarn:silent"
            )
            Seq(
              SettingKey(Some(scala211), None) := Const.EmptySeq,
              SettingKey(Some(scala212), None) := Defaults.Scala212Options.filterNot(removedOpts) ++ addedOpts,
              SettingKey(Some(scala213), None) := Defaults.Scala213Options.filterNot(removedOpts) ++ addedOpts,
              SettingKey.Default := Seq(
                "-Ykind-projector",
                "-no-indent",
                "-language:implicitConversions"
              )
            )
          },
          "scalacOptions" -= "-Wconf:any:error",
          "mimaPreviousArtifacts" := Seq(
            SettingKey(Some(scala300), None) := """Set(organization.value %% name.value % "2.2.5", organization.value %% name.value % "2.1.0")""".raw,
            SettingKey.Default := """Set(organization.value %% name.value % "2.2.5", organization.value %% name.value % "2.1.0", organization.value %% name.value % "1.0.0")""".raw
          ),
          "scalacOptions" ++= Seq(
            SettingKey(Some(scala213), None) := Seq(
              // workaround for https://github.com/scala/bug/issues/12103
              "-Xlint:-implicit-recursion"
            ),
            SettingKey.Default := Const.EmptySeq
          ),
          "scalacOptions" ++= {
            val inlineOpts = Seq(
              "-opt:l:inline",
              "-opt-inline-from:izumi.reflect.**"
            )
            Seq(
              SettingKey(Some(scala212), Some(true)) := inlineOpts,
              SettingKey(Some(scala213), Some(true)) := inlineOpts,
              SettingKey.Default := Const.EmptySeq
            )
          },
          // For compatibility with Java 9+ module system;
          // without Automatic-Module-Name, the module name is derived from the jar file which is invalid because of the scalaVersion suffix.
          "packageOptions" in SettingScope.Raw("Compile / packageBin") +=
            s"""Package.ManifestAttributes("Automatic-Module-Name" -> s"$${organization.value.replaceAll("-",".")}.$${moduleName.value.replaceAll("-",".")}")""".raw
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
        settings = Seq(
          SettingDef.RawSettingDef(
            """Compile / scalacOptions --= Seq("-Ywarn-value-discard","-Ywarn-unused:_", "-Wvalue-discard", "-Wunused:_")""",
            FullSettingScope(SettingScope.Compile, Platform.All)
          )
        )
      ),
      Artifact(
        name = Projects.izumi_reflect_aggregate.izumi_reflect,
        libs = Seq(
          collection_compat in Scope.Provided.all
        ),
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

  final lazy val izumi_reflect: Project = Project(
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
      scala_reflect in Scope.Provided.all.scalaVersion(ScalaVersionScope.AllScala2),
      scala3_compiler in Scope.Provided.all.scalaVersion(ScalaVersionScope.AllScala3),
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
