package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interpreter.*
import esmeta.ty.{*, given}
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import scala.collection.mutable.{Map => MMap}

/** `eval` phase */
case object Eval extends Phase[CFG, State] {
  val name = "eval"
  val help = "evaluates an ECMAScript file."

  val totalErrors: MMap[TypeError, Set[String]] = MMap()

  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): State =
    if (config.multiple) {
      var st = State(cfg, Context(cfg.main))
      for {
        path <- cmdConfig.targets
        file <- walkTree(path)
        filename = file.toString
        if jsFilter(filename)
      } st = run(cfg, config, filename)
      if (config.tyCheck)
        val pw = getPrintWriter(s"$EVAL_LOG_DIR/errors")
        val sorted: Vector[TypeError] =
          totalErrors.iterator.toVector
            .sortBy { case (_, tests) => -tests.size }
            .map(_._1)
        pw.println(s"${sorted.length} type errors detected." + LINE_SEP)
        for { error <- sorted } do {
          pw.println(error)
          pw.println(s"- Found in ${totalErrors(error).size} file(s)")
          val sample = totalErrors(error).head
          pw.println(s"  - sample: ${sample}")
          pw.println(LINE_SEP)
          pw.flush
        }
      st
    } else run(cfg, config, getFirstFilename(cmdConfig, this.name))

  def run(cfg: CFG, config: Config, filename: String): State =
    val interp = new Interpreter(
      cfg.init.fromFile(filename),
      log = config.log,
      detail = config.detail,
      tyCheck = config.tyCheck,
      timeLimit = config.timeLimit,
    )
    val res = interp.result
    if (config.tyCheck) {
      val errors = interp.getTypeErrors
      if (config.multiple) {
        for { error <- errors } do {
          val updated = totalErrors.getOrElse(error, Set()) + filename
          totalErrors += error -> updated
        }
      } else for (error <- errors) println(error.toString + LINE_SEP)
    }
    res

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "multiple",
      BoolOption(_.multiple = _),
      "execute multiple programs (result is the state of the last program).",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "turn on logging mode with detailed information.",
    ),
    (
      "type-check",
      BoolOption(_.tyCheck = _),
      "perform dynamic type checking.",
    ),
  )
  case class Config(
    var timeLimit: Option[Int] = None,
    var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var tyCheck: Boolean = false,
  )
}
