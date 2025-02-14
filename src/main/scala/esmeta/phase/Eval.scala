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

  def errors: Set[TypeError] = errorMap.keys.toSet
  protected def addError(error: TypeError, filename: String): Unit =
    errorMap.get(error) match
      case None      => errorMap += error -> Set(filename)
      case Some(set) => errorMap += error -> (set + filename)
  private var errorMap: Map[TypeError, Set[String]] = Map()

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
        dumpFile(
          name = "detected type errors",
          data = errorMap.toVector
            .sortBy(-_._2.size)
            .map(_._1.toString)
            .mkString(LINE_SEP + LINE_SEP),
          filename = s"$EVAL_LOG_DIR/errors",
        )
      st
    } else run(cfg, config, getFirstFilename(cmdConfig, this.name))

  def run(cfg: CFG, config: Config, filename: String): State =
    val interp = new Interpreter(
      cfg.init.fromFile(filename),
      tyCheck = config.tyCheck,
      log = config.log,
      detail = config.detail,
      timeLimit = config.timeLimit,
    )
    val st = interp.result
    if (config.tyCheck)
      if (config.multiple) interp.errors.foreach(addError(_, filename))
      else interp.errors.foreach(e => println(e.toString + LINE_SEP))
    st

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
      "type-check",
      BoolOption(_.tyCheck = _),
      "perform dynamic type checking.",
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
  )
  case class Config(
    var timeLimit: Option[Int] = None,
    var multiple: Boolean = false,
    var tyCheck: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
  )
}
