package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*

trait CmdBreakDecl { self: Self =>

  /** break command */
  case object CmdBreak
    extends Command(
      "break",
      "Add a break point.",
    ) {

    /** options */
    val options @ List(func, block) = List("func", "block")

    /** run command */
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit = args match {
      case opt :: bp :: _ if options contains opt.substring(1) =>
        Repl.breakpoints += (opt.substring(1) -> bp)
      case _ => println("Inappropriate argument")
    }
  }
}
