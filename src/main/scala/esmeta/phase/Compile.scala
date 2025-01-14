package esmeta.phase

import esmeta.*
import esmeta.ir.Program
import esmeta.compiler.Compiler
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `compile` phase */
case object Compile extends Phase[Spec, Program] {
  val name = "compile"
  val help = "compiles a specification to an IR program."
  def apply(
    spec: Spec,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = {
    val compiler = new Compiler(spec, config.log)
    val program = compiler.result

    // logging mode
    if (config.log)
      // results
      program.dumpTo(COMPILE_LOG_DIR, config.loc)

      // yet expressions
      dumpFile(
        name = "yet expressions",
        data = (for {
          (yet, func) <- program.yets
        } yield s"$yet @ ${func.name}").sorted.mkString(LINE_SEP),
        filename = s"$COMPILE_LOG_DIR/yets",
      )

      // unused manual rules
      dumpFile(
        name = "unused manual rules",
        data = compiler.unusedRules.toList
          .map(_.toString)
          .sorted
          .mkString(LINE_SEP),
        filename = s"$COMPILE_LOG_DIR/unused-rules",
      )

    program
  }
  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "log-with-loc",
      BoolOption((c, b) => { c.log ||= b; c.loc = b }),
      "turn on logging mode with location info.",
    ),
  )
  case class Config(
    var log: Boolean = false,
    var loc: Boolean = false,
  )
}
