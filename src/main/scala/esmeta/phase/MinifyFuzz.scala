package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.spec.util.GrammarGraph
import esmeta.es.util.fuzzer.{Fuzzer, MinifyFuzzer, FSTreeConfig}
import esmeta.js.minifier.Minifier
import esmeta.injector.Injector
import scala.util.*
import esmeta.js.JSEngine
import esmeta.injector.ReturnInjector
import esmeta.interpreter.Interpreter
import java.util.concurrent.atomic.AtomicLong
import scala.collection.parallel.CollectionConverters._
import scala.collection.mutable.Map as MMap
import esmeta.util.SystemUtils.*
import esmeta.es.util.ValidityChecker
import esmeta.ir.Op

case object MinifyFuzz extends Phase[CFG, Coverage] {
  val name = "minify-fuzz"
  val help = "generate ECMAScript programs for fuzzing minifier"

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Coverage =
    config.seed.foreach(setSeed)
    val graph = GrammarGraph(cfg.grammar)
    import graph.*

    val cov = MinifyFuzzer(
      cfg = cfg,
      logInterval = config.logInterval,
      debug = config.debug,
      timeLimit = config.timeLimit,
      trial = config.trial,
      duration = config.duration,
      cp = config.cp,
      init = config.init,
      fsTreeConfig = FSTreeConfig(
        maxSensitivity = config.kFs,
        promotionThreshold = config.proThreshold,
        demotionThreshold = config.demThreshold,
        minTouch = config.fsMinTouch,
        oneSided = config.oneSided,
        isSelective = config.isSelectiveOpt
          .getOrElse {
            println(
              "Sensitivity type is not set. Use selective by default.",
            )
            true
          },
        useSrv = config.useSrv,
        useLocalCorrelation = config.useLocalCorrelation,
        doCleanup = config.doCleanup,
      ),
      keepBugs = config.keepBugs,
      minifyCmd = config.minifier,
      onlineTest = config.onlineTest,
    )

    for (dirname <- config.out) cov.dumpToWithDetail(dirname)

    cov

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "log-interval",
      NumOption((c, k) => c.logInterval = Some(k)),
      "turn on logging mode and set logging interval (default: 600 seconds).",
    ),
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "dump the generated ECMAScript programs to a given directory.",
    ),
    (
      "debug",
      NumOption((c, k) =>
        if (k < 0 || k > 2) error("invalid debug level: please set 0 to 2")
        else c.debug = k,
      ),
      "turn on deug mode with level (0: no-debug, 1: partial, 2: all)",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: 1 second).",
    ),
    (
      "trial",
      NumOption((c, k) => c.trial = Some(k)),
      "set the number of trials (default: INF).",
    ),
    (
      "duration",
      NumOption((c, k) => c.duration = Some(k)),
      "set the maximum duration for fuzzing (default: INF)",
    ),
    (
      "seed",
      NumOption((c, k) => c.seed = Some(k)),
      "set the specific seed for the random number generator (default: None).",
    ),
    (
      "sens-type",
      StrOption((c, s) =>
        c.isSelectiveOpt = Some(
          s match {
            case "selective" => true
            case "uniform"   => false
            case _ =>
              error("invalid sensitivity type: please set selective or uniform")
          },
        ),
      ),
      "set the sensitivity type: selective or uniform",
    ),
    (
      "cp",
      BoolOption(c => c.cp = true),
      "turn on the call-path mode (default: false) (meaningful if k-fs > 0).",
    ),
    (
      "k-fs",
      NumOption((c, k) => c.kFs = k),
      "set the k-value for feature sensitivity (default: 0).",
    ),
    (
      "init",
      StrOption((c, s) => c.init = Some(s)),
      "explicitly use the given init pool",
    ),
    (
      "pro-alpha",
      StrOption((c, k) =>
        c.proThreshold = chiSqDistTable.getOrElse(
          k,
          error(
            "unsupported pro-alpha: use 0.2, 0.1, 0.05, 0.025, 0.01, 0.005, 0.002 or 0.001",
          ),
        ),
      ),
      "set the promotion significant level (default: 0.01).",
    ),
    (
      "dem-alpha",
      StrOption((c, k) =>
        c.demThreshold = chiSqDistTable.getOrElse(
          k,
          error(
            "unsupported dem-alpha: use 0.2, 0.1, 0.05, 0.025, 0.01, 0.005, 0.002 or 0.001",
          ),
        ),
      ),
      "set the demotion significant level (default: 0.05).",
    ),
    (
      "fs-min-touch",
      NumOption((c, k) => c.fsMinTouch = k),
      "set the minimum touch for feature sensitivity (default: 10).",
    ),
    (
      "two-sided",
      BoolOption(c => c.oneSided = false),
      "turn on the two-sided mode for selective feature sensitivity (default: one-sided).",
    ),
    (
      "keep-bugs",
      BoolOption(c => c.keepBugs = true),
      "keep the bugs in the generated programs (default: false).",
    ),
    (
      "minifier",
      StrOption((c, s) => c.minifier = Some(s)),
      "set the minifier to use (default: swc).",
    ),
    (
      "online-test",
      BoolOption(c => c.onlineTest = true),
      "turn on the online test mode (default: false).",
    ),
    (
      "use-cli",
      BoolOption(c => c.useSrv = false),
      "use the CLI to transpile scripts (default: server).",
    ),
    (
      "correlation-type",
      StrOption((c, s) =>
        c.useLocalCorrelation = s match {
          case "local"  => true
          case "global" => false
          case _ =>
            error("invalid correlation type: please set local or global")
        },
      ),
      "set the correlation type: local or global",
    ),
    (
      "no-cleanup",
      BoolOption(c => c.doCleanup = false),
      "clean up minimals and TRs (default: false) WARNING: This will cause # of minimals to increase due to successive promotions and demotions.",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var logInterval: Option[Int] = Some(600), // 10 minute
    var out: Option[String] = None,
    var debug: Int = 0,
    var timeLimit: Option[Int] = Some(1),
    var trial: Option[Int] = None,
    var duration: Option[Int] = None, // INF
    var seed: Option[Int] = None,
    var init: Option[String] = None,
    var isSelectiveOpt: Option[Boolean] = None,
    var kFs: Int = 0,
    var cp: Boolean = false,
    var proThreshold: Double = chiSqDistTable("0.01"),
    var demThreshold: Double = chiSqDistTable("0.05"),
    var fsMinTouch: Int = 10,
    var oneSided: Boolean = true,
    var keepBugs: Boolean = false,
    var minifier: Option[String] = None,
    var onlineTest: Boolean = false,
    var useSrv: Boolean = true,
    var useLocalCorrelation: Boolean = false,
    var doCleanup: Boolean = true,
  )
}
