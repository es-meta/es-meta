package esmeta.analyzer.repl

import esmeta.LINE_SEP
import esmeta.analyzer.*
import esmeta.analyzer.repl.command.*
import esmeta.cfg.*
import esmeta.error.ESMetaError
import esmeta.es.*
import esmeta.util.BaseUtils.*
import org.jline.builtins.Completers.TreeCompleter
import org.jline.builtins.Completers.TreeCompleter.{Node => CNode, node}
import org.jline.reader.*
import org.jline.reader.impl.*
import org.jline.terminal.*
import org.jline.utils.*
import org.jline.utils.InfoCmp.Capability
import scala.Console.*
import scala.collection.mutable.ArrayBuffer
import scala.util.matching.Regex

trait ReplDecl { self: Self =>

  // REPL for static analysis
  object Repl {

    // completer
    private val completer: TreeCompleter =
      TreeCompleter(Command.commands.map(optionNode(_)): _*)
    private def optionNode(cmd: Command) =
      node(cmd.name :: cmd.options.map(argNode(_)): _*)
    private def argNode(opt: String) =
      node(s"-$opt" :: getArgNodes(opt): _*)
    private def getArgNodes(opt: String): List[TreeCompleter.Node] = opt match {
      case CmdBreak.func => cfg.funcs.map(x => node(x.name))
      case CmdBreak.block =>
        (0 until cfg.nodeMap.size).map(x => node(x.toString)).toList
      case CmdInfo.ret => cfg.funcs.map(x => node(x.name))
      case CmdInfo.block =>
        (0 until cfg.nodeMap.size).map(x => node(x.toString)).toList
      case _ => Nil
    }

    // show current status
    def showStatus(cp: Option[ControlPoint]): Unit = cp.map(showStatus)
    def showStatus(cp: ControlPoint): Unit = println(s"[$iter] ${cpInfo(cp)}")
    def cpInfo(cp: ControlPoint, detail: Boolean = false): String =
      getString(cp, CYAN, detail) + (cp match
        case np: NodePoint[_] => LINE_SEP + np.node.toString
        case rp: ReturnPoint  => ""
      )

    // handle when the static analysis is finished
    def finished: Unit = {
      printlnColor(CYAN)(s"- Static analysis finished. (# iter: $iter)")
      setCp(None)
      continue = false
      runDirect
    }

    // jline
    private val terminal: Terminal = TerminalBuilder.builder().build()
    private val reader: LineReader = LineReaderBuilder
      .builder()
      .terminal(terminal)
      .completer(completer)
      .build()
    private val prompt: String = LINE_SEP + s"${MAGENTA}analyzer>${RESET} "

    // show help message at the first time
    lazy val firstHelp: Unit = { CmdHelp.showHelp; println }

    // repl stop
    private var replStop: Boolean = false

    // check whether skip REPL
    def isSkip(cp: ControlPoint): Boolean = jumpTo match {
      case _ if nextEntry => true
      case _ if untilMerged =>
        if (worklist.isEmpty && !merged) true
        else { untilMerged = false; merged = false; false }
      case Some(targetIter) =>
        if (iter < targetIter) true
        else { jumpTo = None; false }
      case _ =>
        continue && !isBreak(cp) && {
          if (replStop) { replStop = false; false }
          else true
        }
    }

    // run REPL
    def apply(cp: ControlPoint): Unit = try {
      if (!isSkip(cp)) {
        setCp(Some(cp))
        continue = false
        runDirect
      }
      transfer(cp)
    } catch {
      case e: ESMetaError =>
        printlnColor(RED)(s"- # iter: $iter")
        showStatus(cp)
        throw e
      case e: Throwable =>
        printlnColor(RED)(s"- unexpectedly terminated (# iter: $iter).")
        showStatus(cp)
        throw e
    }
    def runDirect: Unit = try {
      firstHelp
      showStatus(curCp)
      while ({
        reader.readLine(prompt) match {
          case null => stop
          case line =>
            line.split("\\s+").toList match {
              case Nil | List("") =>
                continue = false
                false
              case name :: args => {
                Command.cmdMap.get(name) match {
                  case Some(cmd) => cmd(curCp, args)
                  case None =>
                    println(s"The command `$name` does not exist. (Try `help`)")
                }
                !continue
              }
            }
        }
      }) {}
    } catch { case e: EndOfFileException => error("stop for debugging") }

    // original control point
    private var origCp: Option[ControlPoint] = None

    // current control point
    var curCp: Option[ControlPoint] = None

    // set current control point
    def setCp(cpOpt: Option[ControlPoint]) = {
      origCp = cpOpt
      curCp = cpOpt
    }
    def moveCp(cp: ControlPoint) = curCp = Some(cp)
    def restoreCp() = curCp = origCp

    // continue option
    var continue: Boolean = replContinue

    // jump point
    var jumpTo: Option[Int] = None

    // jump to the next ECMAScript entry
    var nextEntry: Boolean = false

    // jump to when the analysis result has an imprecise value
    var untilImprec: Boolean = false

    // jump to when the analysis result is merged
    var untilMerged: Boolean = false
    var merged: Boolean = false

    // break points
    val breakpoints = ArrayBuffer[(String, String)]()
    private def isBreak(cp: ControlPoint): Boolean = cp match {
      case NodePoint(func, node, _) if func.entry == node =>
        breakpoints.exists {
          case (CmdBreak.func, name) => name == cfg.funcOf(node).name
          case (CmdBreak.block, uid) => uid.toInt == node.id
          case _                     => false
        }
      case np: NodePoint[_] if untilImprec && getState(np).hasImprec =>
        untilImprec = false; true
      case np @ NodePoint(_, node, _) =>
        breakpoints.exists {
          case (CmdBreak.block, uid) => uid.toInt == node.id
          case _                     => false
        }
      case _ => false
    }

    // stop
    def stop: Boolean = { breakpoints.clear(); continue = true; false }
  }
}
