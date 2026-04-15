"Providers for Scala compilation rules."

ScalaInfo = provider(
    doc = "Information about a compiled Scala library.",
    fields = {
        "output_jar": "File: the compiled JAR (classes, sjsir, nir, etc.)",
        "compile_jars": "depset of Files: JARs needed on the compile classpath (this jar + transitive)",
        "runtime_jars": "depset of Files: JARs needed at runtime (this jar + transitive)",
        "scala_version": "string: full Scala version used for compilation",
        "platform": "string: 'jvm', 'js', or 'native'",
    },
)
