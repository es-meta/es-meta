package esmeta.analyzer.repl.command

import esmeta.analyzer.*
import esmeta.analyzer.repl.*
import esmeta.cfg.*
import esmeta.ir.Expr
import esmeta.util.BaseUtils.*

trait CmdPrintDecl { self: Self =>

// print command
  case object CmdPrint
    extends Command(
      "print",
      "Print specific information",
    ) {
    // options
    val options @ List(reachLoc, ret, expr) =
      List("reach-loc", "return", "expr")

    // run command
    def apply(
      cpOpt: Option[ControlPoint],
      args: List[String],
    ): Unit =
      val cp = cpOpt.getOrElse(runJobsRp)
      args match
        case s"-${`ret`}" :: _ => println(getString(cp.toReturnPoint))
        case s"-${`expr`}" :: rest =>
          val str = rest.mkString(" ")
          cp match
            case np: NodePoint[Node] =>
              given NodePoint[Node] = np
              val st = getState(np)
              val (v, _) = transfer.transfer(Expr.from(str))(st)
              println(v.getString(st))

            case rp: ReturnPoint =>
              println("cannot evaluate expression in return point")
        case _ => println("Inappropriate argument")
  }
}
