package generated
object TestRunner {
  case class TestResult(name: String, suite: String, durationMs: Long, failure: Option[(String, String)])
  def main(args: Array[String]): Unit = {
    val suites = List[org.scalatest.Suite](
