package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.interpreter.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*
import esmeta.es.util.Coverage.*
import esmeta.state.Feature

/** `eval` phase */
case object Eval extends Phase[CFG, State] {
  val name = "eval"
  val help = "evaluates an ECMAScript file."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): State =
    if (config.multiple)
      var st = State(cfg, Context(cfg.main))
      for {
        path <- cmdConfig.targets
        file <- walkTree(path)
        filename = file.toString
        if jsFilter(filename)
      } st = run(cfg, config, filename)
      st
    else run(cfg, config, getFirstFilename(cmdConfig, this.name))

  def run(cfg: CFG, config: Config, filename: String): State =
    val interp = Interp(
      cfg.init.fromFile(filename),
      cp = false,
      timeLimit = config.timeLimit,
    )
    val res = interp.result
    var featureStacks: Set[List[Feature]] = Set()
    for (nv <- interp.touchedNodeViews) {
      val (NodeView(_, view), _) = nv
      view.foreach {
        case (enclosing, feature, _) =>
          val featureStack = (feature :: enclosing)
          featureStacks += featureStack
      }
    }
    for (cv <- interp.touchedCondViews) {
      val (CondView(_, view), _) = cv
      view.foreach {
        case (enclosing, feature, _) =>
          val featureStack = (feature :: enclosing)
          featureStacks += featureStack
      }
    }
    for (featureStack <- featureStacks) {
      val featureStackStr = featureStack.map(_.func.name).mkString("\n")
      println(featureStackStr)
      println()
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
      BoolOption(c => c.multiple = true),
      "execute multiple programs (result is the state of the last program).",
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
  )
  case class Config(
    var timeLimit: Option[Int] = None,
    var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
  )
}
