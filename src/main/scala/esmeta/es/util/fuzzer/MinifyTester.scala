package esmeta.es.util.fuzzer

import scala.util.*
import esmeta.cfg.CFG
import esmeta.js.minifier.Minifier
import esmeta.injector.Injector
import esmeta.injector.NormalTag
import esmeta.js.JSEngine
import esmeta.es.util.delta.DeltaDebugger
import esmeta.injector.ConformTest
import esmeta.injector.ExitStateExtractor
import esmeta.state.*
import esmeta.es.util.Coverage.*

case class MinifyTesterConfig(
  timeLimit: Option[Int] = Some(1),
  ignoreProperties: List[String] = Nil,
  debugLevel: Int = 0,
)

class MinifyTester(
  cfg: CFG,
  config: MinifyTesterConfig = MinifyTesterConfig(),
  minifyCmd: Option[String],
) {
  val MinifyTesterConfig(timeLimit, ignoreProperties, debugLevel) = config

  def dd(code: String): String =
    test(code) match
      case Some(AssertionFailure(original, minified, injected, reason)) =>
        DeltaDebugger(cfg, test(_).fold(false)(!_.isSuccess), debugLevel > 1)
          .result(original)
      case Some(_: AssertionSuccess) => log("minify-tester", "pass"); code
      case _                         => log("minify-tester", "invalid"); code

  def test(code: String): Option[MinifyTestResult] =
    // Minifier.minifySwc(code)
    Minifier.minify(code, minifyCmd) match
      // transpiler crash ?
      case Failure(exception) =>
        log("minify-tester", s"$exception");
        Some(
          AssertionFailure(
            code,
            "",
            "",
            exception.toString,
          ),
        )
      case Success(minified) => {
        val injected = Try {
          RuntimeCompatInjector.replaceBody(
            cfg,
            code,
            minified,
            defs = true,
            timeLimit = timeLimit,
            ignoreProperties = ignoreProperties,
          )
        }
        log("minify-tester", s"test start")
        log("minify-tester/code", s"\n$code\n")
        log("minify-tester/minified", s"\n$minified\n")
        injected match
          case Success(i) if i.exitTag == NormalTag =>
            val injected = i.toString
            JSEngine.runGraal(injected, timeLimit) match
              // minified program passes assertions
              case Success(v) if v.isEmpty =>
                log("minify-tester", "assertion pass");
                Some(
                  AssertionSuccess(
                    code,
                    minified,
                    injected,
                  ),
                )
              // minified program fails on assertions
              case Success(reason) =>
                log("minify-tester", s"assertion failure with:\n$reason\n")
                Some(
                  AssertionFailure(
                    code,
                    minified,
                    injected,
                    reason,
                  ),
                )
              // minified program throws exception
              // TODO(@hyp3rflow): we have to mask span in exception description
              case Failure(e) =>
                val exception = e.getMessage
                log("minify-tester", s"exception throws with:\n$exception\n")
                Some(
                  ExceptionFailure(
                    code,
                    minified,
                    injected,
                    exception,
                  ),
                )
          case Success(i) =>
            // exit tag is not normal
            val originalExitTag = i.exitTag
            log("minify-tester", s"exit tag is not normal: $originalExitTag")
            val interpOriginal = Interp(
              cfg.init.from(code),
              cp = false,
              timeLimit = timeLimit,
            )
            val interpMinified = Interp(
              cfg.init.from(minified),
              cp = false,
              timeLimit = timeLimit,
            )
            val resOriginal = interpOriginal.result
            val resMinified = interpMinified.result
            if resOriginal == resMinified then
              log("minify-tester", "exit tag is not normal but result is same")
              Some(
                AssertionSuccess(
                  code,
                  minified,
                  injected.toString,
                ),
              )
            else
              log(
                "minify-tester",
                "exit tag is not normal and result is different",
              )
              Some(
                AssertionFailure(
                  code,
                  minified,
                  injected.toString,
                  s"exit tag is not normal: $originalExitTag",
                ),
              )
          case Failure(exception) =>
            log("minify-tester", exception.toString); None
      }

  private def log(label: String, description: String) =
    if (debugLevel > 1) println(s"[$label] $description")

  private def info(label: String, description: String) =
    if (debugLevel > 0) System.err.println(s"[$label] $description")
}

sealed trait MinifyTestResult {
  def original: String
  def minified: String
  def injected: String

  def tag: String

  def isSuccess = this match
    case _: AssertionSuccess => true
    case _                   => false

  def getReason: Option[String] = this match
    case AssertionFailure(_, _, _, reason)    => Some(reason)
    case ExceptionFailure(_, _, _, exception) => Some(exception)
    case _                                    => None
}
case class AssertionSuccess(
  original: String,
  minified: String,
  injected: String,
) extends MinifyTestResult {
  def tag: String = "AssertionSuccess"
}
case class AssertionFailure(
  original: String,
  minified: String,
  injected: String,
  reason: String,
) extends MinifyTestResult {
  def tag: String = "AssertionFailure"
}
case class ExceptionFailure(
  original: String,
  minified: String,
  injected: String,
  exception: String,
) extends MinifyTestResult {
  def tag: String = "ExceptionFailure"
}

// this injector intentionally ignores runtime details
object RuntimeCompatInjector {
  def replaceBody(
    cfg: CFG,
    src: String,
    body: String,
    defs: Boolean = false,
    timeLimit: Option[Int] = None,
    log: Boolean = false,
    ignoreProperties: List[String] = Nil,
  ): ConformTest =
    val exitSt = ExitStateExtractor(cfg.init.from(src), timeLimit).result
    new Injector(
      cfg,
      exitSt,
      body,
      defs,
      log,
      ignoreProperties,
    ) {
      override def handlePropKeys(addr: Addr, path: String): Unit =
        exitSt(addr) match
          case obj @ RecordObj(tname, _)
              if tname == "ECMAScriptFunctionObject" =>
          // ignore assertions for checking property keys of user-defined function
          case _ => super.handlePropKeys(addr, path)

    }.conformTest
}
