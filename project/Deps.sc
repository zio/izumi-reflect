import $ivy.`io.7mind.izumi.sbt::sbtgen:0.0.50`
import izumi.sbtgen._
import izumi.sbtgen.model._

object Izumi {

  object V {
    val collection_compat = Version.VExpr("V.collection_compat")
    val kind_projector = Version.VExpr("V.kind_projector")
    val scalatest = Version.VExpr("V.scalatest")
    // val cats = Version.VExpr("V.cats")
    // val cats_effect = Version.VExpr("V.cats_effect")
    // val zio = Version.VExpr("V.zio")
    // val zio_interop_cats = Version.VExpr("V.zio_interop_cats")
    // val circe = Version.VExpr("V.circe")
    // val circe_generic_extras = Version.VExpr("V.circe_generic_extras")
    // val circe_derivation = Version.VExpr("V.circe_derivation")
    // val circe_config = Version.VExpr("V.circe_config")
    // val jawn = Version.VExpr("V.jawn")
    // val http4s = Version.VExpr("V.http4s")
    // val classgraph = Version.VExpr("V.classgraph")
    // val slf4j = Version.VExpr("V.slf4j")
    // val typesafe_config = Version.VExpr("V.typesafe_config")
    // val cglib_nodep = Version.VExpr("V.cglib_nodep")
    // val scala_java_time = Version.VExpr("V.scala_java_time")
    // val docker_java = Version.VExpr("V.docker_java")
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
    Entrypoint.main(izumi, settings, Seq("-o", ".") ++ args)
  }

  val settings = GlobalSettings(
    groupId = "io.7mind.izumi",
    sbtVersion = None,
    scalaJsVersion = PV.scala_js_version,
    scalaNativeVersion = PV.scala_native_version,
    crossProjectVersion = PV.crossproject_version,
    bundlerVersion = PV.scalajs_bundler_version,
  )

  object Deps {
    final val collection_compat = Library("org.scala-lang.modules", "scala-collection-compat", V.collection_compat, LibraryType.Auto)
    final val scalatest = Library("org.scalatest", "scalatest", V.scalatest, LibraryType.Auto) in Scope.Test.all

    // final val cats_core = Library("org.typelevel", "cats-core", V.cats, LibraryType.Auto)
    // final val cats_effect = Library("org.typelevel", "cats-effect", V.cats_effect, LibraryType.Auto)
    // final val cats_all = Seq(
    //   cats_core,
    //   cats_effect,
    // )

    // final val circe_core = Library("io.circe", "circe-core", V.circe, LibraryType.Auto)
    // final val circe_derivation = Library("io.circe", "circe-derivation", V.circe_derivation, LibraryType.Auto)
    // final val circe = Seq(
    //   circe_core,
    //   Library("io.circe", "circe-parser", V.circe, LibraryType.Auto),
    //   Library("io.circe", "circe-literal", V.circe, LibraryType.Auto),
    //   Library("io.circe", "circe-generic-extras", V.circe_generic_extras, LibraryType.Auto),
    //   circe_derivation,
    // ).map(_ in Scope.Compile.all)
    // final val circe_config = Library("io.circe", "circe-config", V.circe_config, LibraryType.Auto)

    // final val zio_core = Library("dev.zio", "zio", V.zio, LibraryType.Auto)
    // final val zio_interop_cats = Library("dev.zio", "zio-interop-cats", V.zio_interop_cats, LibraryType.Auto)
    // final val zio_all = Seq(
    //   zio_core,
    //   zio_interop_cats,
    // )

    // final val typesafe_config = Library("com.typesafe", "config", V.typesafe_config, LibraryType.Invariant) in Scope.Compile.all
    // final val jawn = Library("org.typelevel", "jawn-parser", V.jawn, LibraryType.AutoJvm)

    // final val scala_sbt = Library("org.scala-sbt", "sbt", Version.VExpr("sbtVersion.value"), LibraryType.Invariant)
    // final val scala_compiler = Library("org.scala-lang", "scala-compiler", Version.VExpr("scalaVersion.value"), LibraryType.Invariant)
    // final val scala_library = Library("org.scala-lang", "scala-library", Version.VExpr("scalaVersion.value"), LibraryType.Invariant)
    final val scala_reflect = Library("org.scala-lang", "scala-reflect", Version.VExpr("scalaVersion.value"), LibraryType.Invariant)

    // final val cglib_nodep = Library("cglib", "cglib-nodep", V.cglib_nodep, LibraryType.Invariant) in Scope.Compile.jvm

    final val projector = Library("org.typelevel", "kind-projector", V.kind_projector, LibraryType.Invariant)
      .more(LibSetting.Raw("cross CrossVersion.full"))

    // final val fast_classpath_scanner = Library("io.github.classgraph", "classgraph", V.classgraph, LibraryType.Invariant) in Scope.Compile.jvm
    // final val scala_java_time = Library("io.github.cquiroz", "scala-java-time", V.scala_java_time, LibraryType.Auto) in Scope.Compile.js

    // final val slf4j_api = Library("org.slf4j", "slf4j-api", V.slf4j, LibraryType.Invariant) in Scope.Compile.jvm
    // final val slf4j_simple = Library("org.slf4j", "slf4j-simple", V.slf4j, LibraryType.Invariant) in Scope.Test.jvm

    // final val http4s_client = Seq(
    //   Library("org.http4s", "http4s-blaze-client", V.http4s, LibraryType.Auto),
    // )

    // val http4s_server = Seq(
    //   Library("org.http4s", "http4s-dsl", V.http4s, LibraryType.Auto),
    //   Library("org.http4s", "http4s-circe", V.http4s, LibraryType.Auto),
    //   Library("org.http4s", "http4s-blaze-server", V.http4s, LibraryType.Auto),
    // )

    // val http4s_all = (http4s_server ++ http4s_client)

    // val docker_java = Library("com.github.docker-java", "docker-java", V.docker_java, LibraryType.Invariant)
  }

  import Deps._

  // DON'T REMOVE, these variables are read from CI build (build.sh)
  final val scala211 = ScalaVersion("2.11.12")
  final val scala212 = ScalaVersion("2.12.10")
  final val scala213 = ScalaVersion("2.13.1")

  object Groups {
    final val fundamentals = Set(Group("fundamentals"))
    // final val docs = Set(Group("docs"))
    // final val sbt = Set(Group("sbt"))
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
      final val id = ArtifactId("izumi")
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
          "scmInfo" in SettingScope.Build := """Some(ScmInfo(url("https://github.com/7mind/izumi"), "scm:git:https://github.com/7mind/izumi.git"))""".raw,
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
              "-opt-inline-from:izumi.**",
            ),
            SettingKey(Some(scala213), Some(true)) := Seq(
              "-opt:l:inline",
              "-opt-inline-from:izumi.**",
            ),
            SettingKey.Default := Const.EmptySeq
          ),
        )

    }

    object fundamentals {
      final val id = ArtifactId("fundamentals")
      final val basePath = Seq("fundamentals")

      final val collections = ArtifactId("fundamentals-collections")
      final val platform = ArtifactId("fundamentals-platform")
      final val functional = ArtifactId("fundamentals-functional")

      final val reflection = ArtifactId("izumi-reflect")

      final val thirdpartyBoopickleShaded = ArtifactId("izumi-reflect-thirdparty-boopickle-shaded")

      final lazy val basics = Seq(
        platform,
        collections,
        functional,
      ).map(_ in Scope.Runtime.all)
    }

    // object docs {
    //   final val id = ArtifactId("doc")
    //   final val basePath = Seq("doc")

    //   final lazy val microsite = ArtifactId("microsite")
    // }

    // object sbtplugins {
    //   final val id = ArtifactId("sbt-plugins")
    //   final val basePath = Seq("sbt-plugins")

    //   final val settings = Seq(
    //     "sbtPlugin" := true,
    //   )

    //   final lazy val izumi_deps = ArtifactId("sbt-izumi-deps")
    // }

  }

  final val forkTests = Seq(
    "fork" in (SettingScope.Test, Platform.Jvm) := true,
  )

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
        name = Projects.fundamentals.collections,
        libs = Seq.empty,
        depends = Seq.empty,
      ),
      Artifact(
        name = Projects.fundamentals.platform,
        libs = Seq(
          scala_reflect in Scope.Provided.all,
        ),
        depends = Seq(
          Projects.fundamentals.collections in Scope.Compile.all
        ),
        settings = Seq(
            "npmDependencies" in (SettingScope.Test, Platform.Js) ++= Seq("hash.js" -> "1.1.7"),
          ) ++ crossScalaSources,
        plugins = Plugins(Seq(Plugin("ScalaJSBundlerPlugin", Platform.Js))),
      ),
      Artifact(
        name = Projects.fundamentals.functional,
        libs = Seq.empty,
        depends = Seq.empty,
      ),
      Artifact(
        name = Projects.fundamentals.thirdpartyBoopickleShaded,
        libs = Seq(scala_reflect in Scope.Provided.all),
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
        depends = Seq(
          Projects.fundamentals.platform,
          Projects.fundamentals.functional,
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

  // final val allMonadsOptional = (cats_all ++ Seq(zio_core)).map(_ in Scope.Optional.all)
  // final val allMonadsTest = (cats_all ++ Seq(zio_core)).map(_ in Scope.Test.all)

  val all = Seq(fundamentals)

  // final lazy val docs = Aggregate(
  //   name = Projects.docs.id,
  //   artifacts = Seq(
  //     Artifact(
  //       name = Projects.docs.microsite,
  //       libs = (cats_all ++ zio_all ++ http4s_all).map(_ in Scope.Compile.all),
  //       depends = all.flatMap(_.artifacts).map(_.name in Scope.Compile.all).distinct,
  //       settings = Seq(
  //         "coverageEnabled" := false,
  //         "skip" in SettingScope.Raw("publish") := true,
  //         "DocKeys.prefix" :=
  //           """{if (isSnapshot.value) {
  //           "latest/snapshot"
  //         } else {
  //           "latest/release"
  //         }}""".raw,
  //         "previewFixedPort" := "Some(9999)".raw,
  //         "git.remoteRepo" := "git@github.com:7mind/izumi-microsite.git",
  //         "classLoaderLayeringStrategy" in SettingScope.Raw("Compile") := "ClassLoaderLayeringStrategy.Flat".raw,
  //         "mdocIn" := """baseDirectory.value / "src/main/tut"""".raw,
  //         "sourceDirectory" in SettingScope.Raw("Paradox") := "mdocOut.value".raw,
  //         "mdocExtraArguments" ++= Seq(" --no-link-hygiene"),
  //         "mappings" in SettingScope.Raw("SitePlugin.autoImport.makeSite") :=
  //           """{
  //           (mappings in SitePlugin.autoImport.makeSite)
  //             .dependsOn(mdoc.toTask(" "))
  //             .value
  //         }""".raw,
  //         "version" in SettingScope.Raw("Paradox") := "version.value".raw,
  //         SettingDef.RawSettingDef("ParadoxMaterialThemePlugin.paradoxMaterialThemeSettings(Paradox)"),
  //         SettingDef.RawSettingDef("addMappingsToSiteDir(mappings in(ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc)"),
  //         SettingDef.RawSettingDef(
  //           "unidocProjectFilter in(ScalaUnidoc, unidoc) := inAggregates(`fundamentals-jvm`, transitive = true) || inAggregates(`distage-jvm`, transitive = true) || inAggregates(`logstage-jvm`, transitive = true)"
  //         ),
  //         SettingDef.RawSettingDef(
  //           """paradoxMaterialTheme in Paradox ~= {
  //           _.withCopyright("7mind.io")
  //             .withRepository(uri("https://github.com/7mind/izumi"))
  //           //        .withColor("222", "434343")
  //         }"""),
  //         "siteSubdirName" in SettingScope.Raw("ScalaUnidoc") := """s"${DocKeys.prefix.value}/api"""".raw,
  //         "siteSubdirName" in SettingScope.Raw("Paradox") := """s"${DocKeys.prefix.value}/doc"""".raw,
  //         SettingDef.RawSettingDef(
  //           """paradoxProperties ++= Map(
  //           "scaladoc.izumi.base_url" -> s"/${DocKeys.prefix.value}/api/",
  //           "scaladoc.base_url" -> s"/${DocKeys.prefix.value}/api/",
  //           "izumi.version" -> version.value,
  //         )"""),
  //         SettingDef.RawSettingDef(
  //           """excludeFilter in ghpagesCleanSite :=
  //           new FileFilter {
  //             def accept(f: File): Boolean = {
  //               (f.toPath.startsWith(ghpagesRepository.value.toPath.resolve("latest")) && !f.toPath.startsWith(ghpagesRepository.value.toPath.resolve(DocKeys.prefix.value))) ||
  //                 (ghpagesRepository.value / "CNAME").getCanonicalPath == f.getCanonicalPath ||
  //                 (ghpagesRepository.value / ".nojekyll").getCanonicalPath == f.getCanonicalPath ||
  //                 (ghpagesRepository.value / "index.html").getCanonicalPath == f.getCanonicalPath ||
  //                 (ghpagesRepository.value / "README.md").getCanonicalPath == f.getCanonicalPath ||
  //                 f.toPath.startsWith((ghpagesRepository.value / "media").toPath) ||
  //                 f.toPath.startsWith((ghpagesRepository.value / "v0.5.50-SNAPSHOT").toPath)
  //             }
  //           }"""
  //         )
  //       ),
  //       plugins = Plugins(
  //         enabled = Seq(
  //           Plugin("ScalaUnidocPlugin"),
  //           Plugin("ParadoxSitePlugin"),
  //           Plugin("SitePlugin"),
  //           Plugin("GhpagesPlugin"),
  //           Plugin("ParadoxMaterialThemePlugin"),
  //           Plugin("PreprocessPlugin"),
  //           Plugin("MdocPlugin")
  //         ),
  //         disabled = Seq(Plugin("ScoverageSbtPlugin"))
  //       )
  //     ),
  //   ),
  //   pathPrefix = Projects.docs.basePath,
  //   groups = Groups.docs,
  //   defaultPlatforms = Targets.jvm,
  //   dontIncludeInSuperAgg = true,
  // )

  // final lazy val sbtplugins = Aggregate(
  //   name = Projects.sbtplugins.id,
  //   artifacts = Seq(
  //     Artifact(
  //       name = Projects.sbtplugins.izumi_deps,
  //       libs = Seq.empty,
  //       depends = Seq.empty,
  //       settings = Projects.sbtplugins.settings ++ Seq(
  //         SettingDef.RawSettingDef("""withBuildInfo("izumi.sbt.deps", "Izumi")""")
  //       ),
  //       plugins = Plugins(
  //         enabled = Seq.empty,
  //         disabled = Seq(Plugin("ScoverageSbtPlugin")),
  //       )
  //     ),
  //   ),
  //   pathPrefix = Projects.sbtplugins.basePath,
  //   groups = Groups.sbt,
  //   defaultPlatforms = Targets.jvmSbt,
  //   enableProjectSharedAggSettings = false,
  //   dontIncludeInSuperAgg = false,
  // )

  val izumi: Project = Project(
    name = Projects.root.id,
    aggregates = Seq(
      fundamentals,
      // docs,
      // sbtplugins,
    ),
    topLevelSettings = Projects.root.settings,
    sharedSettings = Projects.root.sharedSettings,
    sharedAggSettings = Projects.root.sharedAggSettings,
    rootSettings = Projects.root.rootSettings,
    imports = Seq.empty,
    globalLibs = Seq(
      ScopedLibrary(projector, FullDependencyScope(Scope.Compile, Platform.All), compilerPlugin = true),
      collection_compat in Scope.Compile.all,
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
