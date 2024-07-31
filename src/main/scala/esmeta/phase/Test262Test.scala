package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.*
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.Coverage
import esmeta.test262.{*, given}
import esmeta.test262.util.TestFilter
import java.io.File
import java.util.concurrent.TimeoutException

/** `test262-test` phase */
case object Test262Test extends Phase[CFG, Summary] {
  val name = "test262-test"
  val help = "tests Test262 tests with harness files (default: tests/test262)."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Summary =
    // set test mode
    TEST_MODE = true

    // get target version of Test262
    val version = Test262.getVersion(config.target)
    val test262 = Test262(version, cfg, config.withYet)
    val targets =
      if (cmdConfig.targets.isEmpty) None
      else Some(cmdConfig.targets)

    // run test262 eval test
    val summary = test262.evalTest(
      targets,
      config.features,
      config.log,
      config.progress,
      config.coverage,
      config.timeLimit,
      config.concurrent,
      config.verbose,
    )

    // if summary has failed test case, throws an exception
    if (summary.failCount > 0) throw Test262Fail(summary.fail)

    // return summary
    summary

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target git version of Test262 (default: current version).",
    ),
    (
      "features",
      StrListOption((c, s) => c.features = Some(s)),
      "set the target features of Test262.",
    ),
    (
      "progress",
      BoolOption(c => c.progress = true),
      "show progress bar.",
    ),
    (
      "coverage",
      BoolOption(c => c.coverage = true),
      "measure node/branch coverage in CFG of ECMA-262.",
    ),
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "with-yet",
      BoolOption(c => c.withYet = true),
      "test with currently ignored tests because of unknown issues.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "concurrent",
      NumOption((c, k) =>
        c.concurrent = if (k <= 1) then CP.Single else CP.Fixed(k),
      ),
      "set the number of thread to use concurrently (default: no concurrent mode).",
    ),
    (
      "verbose",
      BoolOption(c => c.verbose = true),
      "turn on verbose mode.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var coverage: Boolean = false,
    var progress: Boolean = false,
    var timeLimit: Option[Int] = None,
    var withYet: Boolean = false,
    var log: Boolean = false,
    var concurrent: CP = CP.Single,
    var features: Option[List[String]] = None,
    var verbose: Boolean = false,
  )
}
