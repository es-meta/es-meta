package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.es.util.USE_STRICT
import esmeta.es.Script
import esmeta.interpreter.*
import scala.util.*
import java.util.concurrent.atomic.AtomicLong
import esmeta.util.SystemUtils.*
import scala.collection.parallel.CollectionConverters._
import esmeta.es.util.fuzzer.*
import esmeta.es.util.fuzzer.MinifyTester
import esmeta.injector.*
import esmeta.state.State
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.js.JSEngine
import esmeta.js.minifier.Minifier
import esmeta.es.util.delta.DeltaDebugger
import scala.collection.mutable.{Map => MMap, Set => MSet}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.mutator.TracerExprMutator
import esmeta.js.minifier.Minifier.minifyCmd

case object TestMinimals extends Phase[CFG, Unit] {
  val name = "test-minimals"
  val help = "test minimals"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    println("Test minimals")

    val baseDir = getFirstFilename(cmdConfig, "test-minimals")

    val tracerExprMutator = TracerExprMutator(using cfg)
    val tracerInjector = TracerInjector(using cfg)

    val scriptList = listFiles(s"$baseDir/minimal").flatMap { minimal =>
      val name = minimal.getName
      if jsFilter(name) then
        val code = readFile(minimal.getPath).linesIterator
          .filterNot(_.trim.startsWith("//"))
          .mkString("\n")
          .strip
          .drop(USE_STRICT.length)
          .strip
        Some(Script(code, name))
      else None
    }
    val totalCount = scriptList.size

    val minifierName = config.minifier match
      case Some("swc") | Some("Swc") | None => "swc"
      case Some("terser") | Some("Terser")  => "terser"
      case Some("babel") | Some("Babel")    => "babel"
      case Some("swcES2015") | Some("SwcES2015") =>
        "swcES2015"
      case _ => throw new Exception("Unsupported minifier")

    val baseLogDir =
      config.deltaDebug match
        case true =>
          s"$baseDir/minimal/delta/$minifierName-dd"
        case false =>
          s"$baseDir/minimal/delta/$minifierName"

    mkdir(baseLogDir)

    val minifyFuzzer =
      new MinifyFuzzer(
        cfg,
        targetFeatSetConfig = TargetFeatureSetConfig(),
        minifyCmd = config.minifier,
      )
    val bugCount =
      if config.deltaDebug then
        minifyFuzzer.testMinimalWithDeltaDebug(
          scriptList,
          baseLogDir,
        )
      else minifyFuzzer.testMinimal(scriptList, baseLogDir)

    println(s"Total: $totalCount, Bugs: $bugCount")
    println(s"Bug rate: ${bugCount.toDouble / totalCount * 100}%")
    if config.out.isDefined then
      val out = config.out.get
      val json = Json.obj(
        "total" -> totalCount.asJson,
        "bugs" -> bugCount.asJson,
        "bugRate" -> (bugCount.toDouble / totalCount * 100).asJson,
      )
      dumpJson(
        json,
        out,
      )

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
    (
      "eval-time-limit",
      NumOption((c, k) => c.evalTimeLimit = Some(k)),
      "set the evaluation time limit in seconds (default: no limit).",
    ),
    (
      "inject-time-limit",
      NumOption((c, k) => c.injectTimeLimit = Some(k)),
      "set the injection time limit in seconds (default: no limit).",
    ),
    (
      "test-time-limit",
      NumOption((c, k) => c.testTimeLimit = Some(k)),
      "set the test time limit in seconds (default: no limit).",
    ),
    (
      "minifier",
      StrOption((c, s) => c.minifier = Some(s)),
      "minifier to use.",
    ),
    (
      "delta-debug",
      BoolOption((c) => c.deltaDebug = true),
      "use delta debugging.",
    ),
  )

  class Config(
    var out: Option[String] = None,
    var evalTimeLimit: Option[Int] = None,
    var injectTimeLimit: Option[Int] = None,
    var testTimeLimit: Option[Int] = None,
    var minifier: Option[String] = None,
    var deltaDebug: Boolean = false,
  )
}
