package esmeta.phase

import esmeta.*
import esmeta.analyzer.*
import esmeta.analyzer.TypeAnalyzer.Ignore
import esmeta.analyzer.domain
import esmeta.cfg.{CFG, Func}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*

/** `tycheck` phase */
case object TypeCheck extends Phase[CFG, AbsSemantics] {
  val name = "tycheck"
  val help = "performs a type analysis of ECMA-262."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): AbsSemantics =
    val ignorePath = config.ignorePath match
      case None => cfg.spec.manualInfo.tycheckIgnore
      case path => path
    val ignore = ignorePath.fold(Ignore())(Ignore(_, config.updateIgnore))
    val tags = cfg.spec.manualInfo.fingerprintTags
    TypeAnalyzer(cfg, config.level)(
      target = config.target,
      ignore = ignore,
      tags = tags,
      log = config.log,
      detail = config.detail,
      silent = config.silent || cmdConfig.silent,
    )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "target",
      StrOption((c, s) => c.target = Some(s)),
      "set the target of type analysis with a regular expression pattern.",
    ),
    (
      "level",
      NumOption((c, k) => c.level = k),
      "turn on alarms for type errors whose alarm level is " +
      "lower than or equal to the given number (default: 1)",
    ),
    (
      "repl",
      BoolOption(c => USE_REPL = true),
      "use a REPL for type analysis of ECMA-262.",
    ),
    (
      "repl-continue",
      BoolOption(c => REPL_CONTINUE = true),
      "run `continue` command at startup when using REPL",
    ),
    (
      "ignore",
      StrOption((c, s) => c.ignorePath = Some(s)),
      "ignore type mismatches in algorithms listed in a given JSON file.",
    ),
    (
      "update-ignore",
      BoolOption(c => c.updateIgnore = true),
      "update the given JSON file used in ignoring type mismatches.",
    ),
    (
      "silent",
      BoolOption(c => c.silent = true),
      "turn on silent mode.",
    ),
    (
      "log",
      BoolOption(c => c.log = true),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption(c => { c.log = true; c.detail = true }),
      "turn on logging mode with detailed information.",
    ),
    (
      "logdir",
      StrOption((c, s) => ANALYZE_LOG_DIR = s),
      "set the directory for analyzer logs (log, detail-log).",
    ),
    (
      "tysens",
      BoolOption(c => TY_SENS = true),
      "turn on type sensitivity.",
    ),
  )
  case class Config(
    var target: Option[String] = None,
    var level: Int = 1,
    var ignorePath: Option[String] = None,
    var updateIgnore: Boolean = false,
    var silent: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
  )
}
