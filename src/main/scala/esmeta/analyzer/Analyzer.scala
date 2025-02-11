package esmeta.analyzer

import esmeta.cfg.{util => _, *}
import esmeta.error.*
import esmeta.error.NotSupported.given
import esmeta.es.Ast
import esmeta.ir.{Func => _, util => _, *}
import esmeta.state.{util => _, *}
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.domain.*, Lattice.*

/** static analyzer */
abstract class Analyzer
  extends AbsTransferLikeDecl
  with ControlPointDecl
  with ViewLikeDecl
  with util.Decl
  with repl.Decl {

  // ---------------------------------------------------------------------------
  // Needs to be Implemented
  // ---------------------------------------------------------------------------
  /** control flow graph */
  val cfg: CFG

  /** worklist of control points */
  val worklist: Worklist[ControlPoint]

  /** view abstraction for analysis sensitivities */
  type View <: ViewLike

  /** abstract transfer function */
  type AbsTransfer <: AbsTransferLike
  val transfer: AbsTransfer

  /** check reachability of node points */
  def reachable(np: NodePoint[Node]): Boolean

  /** check reachability of return points */
  def reachable(rp: ReturnPoint): Boolean

  /** lookup for node points */
  def getResult(np: NodePoint[Node]): AbsState =
    npMap.getOrElse(np, AbsState.Bot)

  /** lookup for return points */
  def getResult(rp: ReturnPoint): AbsRet =
    rpMap.getOrElse(rp, AbsRet.Bot)

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: Option[String] = None,
    detail: Boolean = false,
  ): String

  /** logging mode */
  val log: Boolean

  /** logging the current analysis result */
  def logging: Unit

  // ---------------------------------------------------------------------------
  // Abstract Domains
  // ---------------------------------------------------------------------------
  /** value domains */
  val AbsValue: ValueDomain
  type AbsValue <: Printable[AbsValue]
  trait ValueDomain extends Domain {
    type Conc = Value
    type Elem = AbsValue
    extension (elem: AbsValue) {

      /** get string of abstract value with an abstract state */
      def getString(state: AbsState): String
    }
  }

  /** state domains */
  val AbsState: StateDomain
  type AbsState <: Printable[AbsState]
  trait StateDomain extends Domain {
    type Conc = State
    type Elem = AbsState
    extension (elem: AbsState) {

      /** has imprecise elements */
      def hasImprec: Boolean
    }
  }

  /** return value domains */
  val AbsRet: RetDomain
  type AbsRet <: Printable[AbsRet]
  trait RetDomain extends Domain {
    type Conc = (Value, State)
    type Elem = AbsRet
    extension (elem: AbsRet) {

      /** return value */
      def value: AbsValue
    }
  }

  // ---------------------------------------------------------------------------
  // Analysis Results
  // ---------------------------------------------------------------------------
  /** abstract states in each node point */
  var npMap: Map[NodePoint[Node], AbsState] = Map()

  /** abstract states in each return point */
  var rpMap: Map[ReturnPoint, AbsRet] = Map()

  /** abstract states right before calling functions */
  var callInfo: Map[NodePoint[Call], AbsState] = Map()

  /** return edges */
  var retEdges: Map[ReturnPoint, Set[NodePoint[Call]]] = Map()

  /** current control point */
  var curCp: Option[ControlPoint] = None

  /** the number of iterations */
  var iter: Int = 0

  /** count for each control point */
  var counter: Map[ControlPoint, Int] = Map()

  /** set start time of analyzer */
  var startTime: Long = System.currentTimeMillis

  /** analysis time limit */
  val timeLimit: Option[Long] = None

  /** debugging mode */
  val debugMode: Boolean = false

  /** REPL mode */
  val useRepl: Boolean = false

  /** Run continue command at startup when using repl */
  val replContinue: Boolean = false

  /** check period */
  val checkPeriod: Int = 10000

  /** throw exception for not yet compiled expressions */
  val yetThrow: Boolean = false

  /** detailed string */
  val detail: Boolean = false

  /** string with location */
  val location: Boolean = false

  /** stringifier for states */
  val stateStringifier = StateElem.getStringifier((detail, location))

  /** stringifier for CFG */
  val cfgStringifier = CFGElem.getStringifier(detail, location)

  /** stringifier for CFG */
  val irStringifier = IRElem.getStringifier(detail, location)

  // ---------------------------------------------------------------------------
  // Predefined Definitions
  // ---------------------------------------------------------------------------
  given CFG = cfg

  /** analyzer elements */
  trait AnalyzerElem {
    override def toString: String = toString(false, false, false)

    /** stringify with options */
    def toString(
      detail: Boolean = false,
      line: Boolean = false,
      asite: Boolean = false,
    ): String =
      val stringifier = AnalyzerElem.getStringifier(detail, line, asite)
      import stringifier.elemRule
      stringify(this)
  }
  object AnalyzerElem {
    val getStringifier = cached[(Boolean, Boolean, Boolean), Stringifier] {
      Stringifier(_, _, _)
    }
  }

  /** get syntax-directed operation (SDO) */
  val getSdo = cached[(Ast, String), Option[(Ast, Func)]](_.getSdo(_))

  /** monad helper */
  val monad: StateMonad[AbsState] = StateMonad[AbsState]()

  /** RunJobs function */
  val runJobs = cfg.fnameMap("RunJobs")

  /** get return point of RunJobs */
  val runJobsRp = ReturnPoint(runJobs, emptyView)

  /** increase the counter */
  def count(cp: ControlPoint): Unit =
    counter += cp -> (counter.getOrElse(cp, 0) + 1)

  /** get elapsed time of analyzer */
  def elapsedTime: Long = System.currentTimeMillis - startTime

  /** set start time of analyzer */
  def allCPs: Set[ControlPoint] = npMap.keySet ++ rpMap.keySet

  /** set of analyzed functions */
  def analyzedFuncs: Set[Func] = npMap.keySet.map(_.func) ++ analyzedReturns

  /** set of analyzed nodes */
  def analyzedNodes: Set[Node] = npMap.keySet.map(_.node)

  /** set of analyzed function returns */
  def analyzedReturns: Set[Func] = rpMap.keySet.map(_.func)

  /** get string for result of all control points */
  def getStrings: List[String] = getStrings(None, false)
  def getStrings(
    color: Option[String] = None,
    detail: Boolean = false,
  ): List[String] = allCPs.toList.sorted.map(getString(_, color, detail))

  /** get string for result of control points */
  def getString(
    cp: ControlPoint,
    color: String,
    detail: Boolean,
  ): String = getString(cp, Some(color), detail)

  /** not supported */
  def notSupported(msg: String): Nothing = throw NotSupported(msg)
}
