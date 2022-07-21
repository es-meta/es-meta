package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.analyzer.*
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `js-analyze` phase */
case object JSAnalyze extends Phase[CFG, AbsSemantics] {
  val name = "js-analyze"
  val help = "analyzes a JavaScript file."
  def apply(
    cfg: CFG,
    globalConfig: GlobalConfig,
    config: Config,
  ): AbsSemantics = {
    val filename = getFirstFilename(globalConfig, this.name)
    AbsSemantics(cfg, readFile(filename), None, config.execLevel).fixpoint
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "repl",
      BoolOption(c => USE_REPL = true),
      "use a REPL for meta-level static analysis.",
    ),
    (
      "exec-level",
      NumOption((c, k) => {
        if (k < 0 || k > 2) println(s"Invalid execution level: $k")
        c.execLevel = k
      }),
      "use concrete execution to check soundness.",
    ),
  )
  case class Config(var execLevel: Int = 0)
}
