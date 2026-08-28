    )
    var results = List.empty[TestResult]
    var failed = 0
    var passed = 0
    suites.foreach { suite =>
      val suiteName = suite.getClass.getName
      println(s"Running: $suiteName")
      val reporter = new org.scalatest.Reporter {
        def apply(event: org.scalatest.events.Event): Unit = event match {
          case e: org.scalatest.events.TestFailed =>
            failed += 1
            val trace = e.throwable.map { t => val sw = new java.io.StringWriter; t.printStackTrace(new java.io.PrintWriter(sw)); sw.toString }.getOrElse("")
            results = TestResult(e.testName, suiteName, e.duration.getOrElse(0L), Some((e.message, trace))) :: results
            println(s"  FAILED: ${e.testName}: ${e.message}")
          case e: org.scalatest.events.TestSucceeded =>
            passed += 1
            results = TestResult(e.testName, suiteName, e.duration.getOrElse(0L), None) :: results
          case _ =>
        }
      }
      suite.run(None, org.scalatest.Args(reporter, tracker = new org.scalatest.Tracker))
    }
    println(s"Tests: $passed passed, $failed failed")
    // sys.env may not work in all Scala.js environments; use JS interop directly
    val jsEnv = scala.scalajs.js.Dynamic.global.process.env
    val xmlPath = Option(jsEnv.JUNIT_OUTPUT_FILE.asInstanceOf[Any]).map(_.toString).filter(_.nonEmpty).orElse(args.headOption)
    xmlPath.foreach { path =>
      val xml = formatJunitXml(results.reverse)
      val fs = scala.scalajs.js.Dynamic.global.require("fs")
      fs.writeFileSync(path, xml)
      println(s"JUnit XML written to $path")
    }
    if (failed > 0) throw new RuntimeException(s"$failed tests failed")
  }
  private def formatJunitXml(results: List[TestResult]): String = {
    val grouped = results.groupBy(_.suite)
    val sb = new StringBuilder
    sb.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<testsuites>\n")
    grouped.foreach { case (suite, tests) =>
      val failures = tests.count(_.failure.isDefined)
      val time = tests.map(_.durationMs).sum / 1000.0
      sb.append(s"""  <testsuite name="$suite" tests="${tests.size}" failures="$failures" time="$time">\n""")
      tests.foreach { t =>
        val n = t.name.replace("&", "&amp;").replace("<", "&lt;").replace("\"", "&quot;")
        sb.append(s"""    <testcase classname="${t.suite}" name="$n" time="${t.durationMs / 1000.0}">\n""")
        t.failure.foreach { case (msg, trace) =>
          val m = msg.replace("&", "&amp;").replace("<", "&lt;").replace("\"", "&quot;")
          sb.append(s"""      <failure message="$m"><![CDATA[$trace]]></failure>\n""")
        }
        sb.append("    </testcase>\n")
      }
      sb.append("  </testsuite>\n")
    }
    sb.append("</testsuites>\n")
    sb.toString
  }
}
