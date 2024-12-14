package esmeta.cfgBuilder

import esmeta.util.BaseUtils.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.ir.util.AllocSiteSetter
import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.scalajs.js.annotation.JSExport

/** CFG builder */
object CFGBuilder:
  @JSExport("apply")
  def apply(
    program: Program,
    log: Boolean = false,
  ): CFG = new CFGBuilder(program).result

/** extensible helper of CFG builder */
class CFGBuilder(
  program: Program,
  log: Boolean = false,
) {

  /** final result */
  lazy val result: CFG =
    asiteSetter.walk(program)
    for { f <- program.funcs } translate(f)
    val cfg = CFG(funcs.toList)
    cfg.program = program
    cfg

  /** translate IR function to cfg function */
  def translate(irFunc: IRFunc): Unit = {
    // body
    val body = irFunc.body

    // entry node with dummy
    var dummyEntry: Node = Block(-1)

    // previous edges
    var prev: List[(Node, Boolean)] = Nil

    // connect previous edges
    def connect(to: Node, isLoopPred: Boolean = false): Unit =
      if (prev.isEmpty) dummyEntry = to
      for ((node, tf) <- prev)
        node.isLoopPred = isLoopPred
        (node, tf) match
          case (block: Block, _) =>
            block.next = if (block.endsWithReturn) None else Some(to)
          case (call: Call, _)         => call.next = Some(to)
          case (branch: Branch, true)  => branch.thenNode = Some(to)
          case (branch: Branch, false) => branch.elseNode = Some(to)

    // aux branch case
    def auxBranch(inst: Inst): Unit =
      inst match
        case branch: BranchInst =>
          branch match
            case IIf(cond, thenInst, elseInst, isAbruptInst) =>
              val branch = Branch(nextNId, BranchKind.If, cond, isAbruptInst)
              connect(branch.setInst(inst))
              val thenPrev = {
                prev = List((branch, true)); aux(thenInst); prev
              }
              val elsePrev = {
                prev = List((branch, false)); aux(elseInst); prev
              }
              prev = thenPrev ++ elsePrev
            case IWhile(cond, body) =>
              val branch = Branch(nextNId, BranchKind.While, cond)
              connect(branch.setInst(inst), isLoopPred = true)
              prev = List((branch, true)); aux(body); connect(branch)
              prev = List((branch, false))
        case _ => throw Exception("impossible match")

    // aux
    def aux(inst: Inst): Unit = inst match {
      case normal: NormalInst =>
        val block = prev match
          case List((b: Block, _)) => b
          case _                   => val b = Block(nextNId); connect(b); b
        block.insts += normal
        prev = List((block, true))
      case ISeq(insts) => for { i <- insts } aux(i)
      case branch: BranchInst =>
        auxBranch(inst)
      case callInst: CallInst =>
        val call = Call(nextNId, callInst)
        connect(call)
        prev = List((call, true))
    }
    aux(body)

    val entry = if (dummyEntry.id == -1) Block(nextNId) else dummyEntry
    val func = Func(nextFId, irFunc, entry)
    funcs += func
  }

  /** allocation site setter */
  val asiteSetter: AllocSiteSetter = new AllocSiteSetter

  // ---------------------------------------------------------------------------
  // protected helpers
  // ---------------------------------------------------------------------------
  // internal lists of functions
  protected val funcs: ListBuffer[Func] = ListBuffer()

  // function id counter
  private var fidCount: Int = 0
  protected def nextFId: Int = { val fid = fidCount; fidCount += 1; fid }

  // node id counter
  private var nidCount: Int = 0
  protected def nextNId: Int = { val nid = nidCount; nidCount += 1; nid }
}
