package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.injector.*
import esmeta.interpreter.*
import esmeta.ir.{Expr, EParse, EBool}
import esmeta.ty.{*, given}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.Interp
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*

/** coverage measurement of cfg */
case class Coverage(
  cfg: CFG,
  tyCheck: Boolean = false,
  kFs: Int = 0,
  cp: Boolean = false,
  timeLimit: Option[Int] = None,
) {
  import Coverage.{*, given}

  val jsonProtocol: JsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.given

  val tyStringifier = TyElem.getStringifier(true, false)
  import tyStringifier.given

  // minimal scripts
  def minimalScripts: Set[Script] = _minimalScripts
  private var _minimalScripts: Set[Script] = Set()

  // detected type errors
  def errorMap: Map[String, Set[TypeError]] = _errorMap
  private var _errorMap: Map[String, Set[TypeError]] = Map()

  // meta-info of each script
  private var _minimalInfo: Map[String, ScriptInfo] = Map()

  // mapping from nodes/conditions to scripts
  private var nodeViewMap: Map[Node, Map[View, Script]] = Map()
  private var nodeViews: Set[NodeView] = Set()
  private var condViewMap: Map[Cond, Map[View, Script]] = Map()
  private var condViews: Set[CondView] = Set()

  def apply(node: Node): Map[View, Script] = nodeViewMap.getOrElse(node, Map())
  def getScript(nv: NodeView): Option[Script] = apply(nv.node).get(nv.view)

  def apply(cond: Cond): Map[View, Script] = condViewMap.getOrElse(cond, Map())
  def getScript(cv: CondView): Option[Script] = apply(cv.cond).get(cv.view)

  // script reference counter
  private var counter: Map[Script, Int] = Map()
  def size: Int = counter.size

  // target conditional branches
  private var _targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = Map()
  def targetCondViews: Map[Cond, Map[View, Option[Nearest]]] = _targetCondViews

  private lazy val scriptParser = cfg.scriptParser

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script): (State, Boolean, Boolean) =
    runAndCheck(script, scriptParser.from(script.code))

  /** evaluate a given ECMAScript program, update coverage, and return
    * evaluation result with whether it succeeds to increase coverage
    */
  def runAndCheck(script: Script, ast: Ast): (State, Boolean, Boolean) =
    val interp = run(script.code, ast, Some(script.name))
    this.synchronized(check(script, interp))

  /** evaluate a given ECMAScript program */
  def run(script: Script): Interp =
    val (ast, code) = scriptParser.fromWithCode(script.code)
    run(code, ast, Some(script.name))

  /** evaluate a given ECMAScript program */
  def run(code: String): Interp = run(code, scriptParser.from(code), None)

  /** evaluate a given ECMAScript program */
  def run(code: String, ast: Ast, name: Option[String]): Interp =
    val initSt = cfg.init.from(code, ast, name)
    val interp = Interp(initSt, tyCheck, kFs, cp, timeLimit)
    interp.result; interp

  def check(script: Script, interp: Interp): (State, Boolean, Boolean) = {
    val Script(code, _) = script
    val finalSt = interp.result

    var covered = false
    var updated = false
    var blockingScripts: Set[Script] = Set.empty

    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    // update node coverage
    for ((nodeView, nearest) <- interp.touchedNodeViews)
      touchedNodeViews += nodeView -> nearest
      getScript(nodeView) match
        case None => update(nodeView, script); updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
          blockingScripts += originalScript
        case Some(blockScript) => blockingScripts += blockScript

    // update branch coverage
    for ((condView, nearest) <- interp.touchedCondViews)
      touchedCondViews += condView -> nearest
      getScript(condView) match
        case None =>
          update(condView, nearest, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, nearest, script)
          updated = true
          blockingScripts += origScript
        case Some(blockScript) => blockingScripts += blockScript

    if (updated)
      _minimalInfo += script.name -> ScriptInfo(
        // TODO ConformTest.createTest(cfg, finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
      )
      _errorMap += script.name -> interp.errors

    // TODO: impl checkWithBlocking using `blockingScripts`
    (finalSt, updated, covered)
  }

  /** get node coverage */
  def nodeCov: Int = nodeViewMap.size
  def nodeViewCov: Int = nodeViews.size

  /** get branch coverage */
  def branchCov: Int = condViewMap.size
  def branchViewCov: Int = condViews.size

  /** dump results with detail */
  def dumpToWithDetail(baseDir: String, withMsg: Boolean = true): Unit = dumpTo(
    baseDir = baseDir,
    withScripts = true,
    withScriptInfo = true,
    withTargetCondViews = true,
    withUnreachableFuncs = true,
    withMsg = withMsg,
  )

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withScriptInfo: Boolean = false,
    withTargetCondViews: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    withMsg: Boolean = false,
  ): Unit = {
    mkdir(baseDir)
    lazy val orderedNodeViews = nodeViews.toList.sorted
    lazy val orderedCondViews = condViews.toList.sorted
    lazy val getNodeViewsId = orderedNodeViews.zipWithIndex.toMap
    lazy val getCondViewsId = orderedCondViews.zipWithIndex.toMap
    dumpJson(
      CoverageConstructor(kFs, cp, timeLimit),
      s"$baseDir/constructor.json",
    )

    val st = System.nanoTime()
    def elapsedSec = (System.nanoTime() - st) / 1e9
    def log(msg: Any): Unit = if (withMsg) println(s"[${elapsedSec}s] $msg")

    dumpJson(
      name = "node coverage",
      data = nodeViewInfos(orderedNodeViews),
      filename = s"$baseDir/node-coverage.json",
      noSpace = false,
    )
    log("Dumped node coverage")

    dumpJson(
      name = "branch coverage",
      data = condViewInfos(orderedCondViews),
      filename = s"$baseDir/branch-coverage.json",
      noSpace = false,
    )
    log("Dumped branch coverage")

    if (withScripts)
      dumpDir[Script](
        name = "minimal ECMAScript programs",
        iterable = _minimalScripts,
        dirname = s"$baseDir/minimal",
        getName = _.name,
        getData = USE_STRICT + _.code + LINE_SEP,
        remove = true,
      )
      log("Dumped scripts")
    // TODO if (withScriptInfo)
    // TODO   dumpDir[(String, ScriptInfo)](
    // TODO     name = "minimal ECMAScript assertions",
    // TODO     iterable = _minimalInfo,
    // TODO     dirname = s"$baseDir/minimal-assertion",
    // TODO     getName = _._1,
    // TODO     getData = {
    // TODO       case (_, ScriptInfo(test, _, _)) =>
    // TODO         Yaml(
    // TODO           "tag" -> test.exitTag.toString,
    // TODO           "assertions" -> test.assertions.map(_.toString),
    // TODO         )
    // TODO     },
    // TODO     remove = true,
    // TODO   )
    // TODO   log("Dumped assertions")
    if (withTargetCondViews)
      dumpJson(
        name = "target conditional branches",
        data = (for {
          (cond, viewMap) <- _targetCondViews
          (view, _) <- viewMap
        } yield getCondViewsId(CondView(cond, view))).toSeq.sorted.asJson,
        filename = s"$baseDir/target-conds.json",
        noSpace = false,
      )
      log("dumped target conds")
    if (withUnreachableFuncs)
      dumpFile(
        name = "unreachable functions",
        data = cfg.funcs
          .filter(f => !nodeViewMap.contains(f.entry))
          .map(_.name)
          .sorted
          .mkString(LINE_SEP),
        filename = s"$baseDir/unreach-funcs",
      )
      log("dumped unreachable functions")
    if (tyCheck)
      dumpJson(
        name = "minimal detected type errors",
        data = _errorMap
          .filter(_._2.nonEmpty)
          .map((name, errors) =>
            name -> errors.map(_.toString),
          ) // !TODO: Minimize error message
          .asJson,
        filename = s"$baseDir/minimal-errors.json",
        noSpace = false,
      )
      log("Dumped type errors")
  }

  /** conversion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String =
    val app = new Appender
    (app >> "- coverage:").wrap("", "") {
      app :> "- node: " >> nodeCov
      app :> "- branch: " >> branchCov
    }
    if (kFs > 0) (app :> "- sensitive coverage:").wrap("", "") {
      app :> "- node: " >> nodeViewCov
      app :> "- branch: " >> branchViewCov
    }
    app.toString

  /** extension for AST */
  extension (ast: Ast) {

    /** get all child nodes */
    def nodeSet: Set[Ast] =
      var nodes = Set(ast)
      ast match
        case Syntactic(_, _, _, cs) =>
          for {
            child <- cs.flatten
            childNodes = child.nodeSet
          } nodes ++= childNodes
        case _ => /* do nothing */
      nodes
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // update mapping from nodes to scripts
  private def update(nodeView: NodeView, script: Script): Unit =
    val NodeView(node, view) = nodeView
    nodeViews += nodeView
    nodeViewMap += node -> updated(apply(node), view, script)

  // update mapping from conditional branches to scripts
  private def update(
    condView: CondView,
    nearest: Option[Nearest],
    script: Script,
  ): Unit = {
    condViews += condView
    val CondView(cond, view) = condView

    // update target branches
    val neg = condView.neg
    cond.branch match
      case _ if nearest.isEmpty          =>
      case Branch(_, _, EBool(_), _, _)  =>
      case _ if getScript(neg).isDefined => removeTargetCond(neg)
      case _                             => addTargetCond(condView, nearest)

    condViewMap += cond -> updated(apply(cond), view, script)
  }

  // update mapping
  private def updated[View](
    map: Map[View, Script],
    view: View,
    script: Script,
  ): Map[View, Script] =
    // decrease counter of original script
    for (origScript <- map.get(view)) {
      val count = counter(origScript) - 1
      counter += (origScript -> count)
      if (count == 0) {
        counter -= origScript
        _minimalScripts -= origScript
        _minimalInfo -= origScript.name
      }
    }
    // increse counter of new script
    _minimalScripts += script
    counter += script -> (counter.getOrElse(script, 0) + 1)
    map + (view -> script)

  // add a cond to targetConds
  private def addTargetCond(cv: CondView, nearest: Option[Nearest]): Unit =
    val CondView(cond, view) = cv
    val origViews = _targetCondViews.getOrElse(cond, Map())
    val newViews = origViews + (view -> nearest)
    _targetCondViews += cond -> newViews

  // remove a cond from targetConds
  private def removeTargetCond(cv: CondView): Unit =
    val CondView(cond, view) = cv
    for (views <- _targetCondViews.get(cond)) {
      val newViews = views - view
      if (newViews.isEmpty)
        _targetCondViews -= cond
      else
        _targetCondViews += cond -> (views - view)
    }

  // get JSON for node coverage
  private def nodeViewInfos(ordered: List[NodeView]): List[NodeViewInfo] =
    for {
      (nodeView, idx) <- ordered.zipWithIndex
      script <- getScript(nodeView)
    } yield NodeViewInfo(idx, nodeView, script.name)

  // get JSON for branch coverage
  private def condViewInfos(ordered: List[CondView]): List[CondViewInfo] =
    for {
      (condView, idx) <- ordered.zipWithIndex
      script <- getScript(condView)
    } yield CondViewInfo(idx, condView, script.name)
}

object Coverage {
  class Interp(
    initSt: State,
    tyCheck: Boolean,
    kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
  ) extends Interpreter(initSt, tyCheck = tyCheck, timeLimit = timeLimit) {
    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    // override eval for node
    override def eval(node: Node): Unit =
      // record touched nodes
      touchedNodeViews += NodeView(node, getView(node)) -> getNearest
      super.eval(node)

    // override branch move
    override def moveBranch(branch: Branch, b: Boolean): Unit =
      // record touched conditional branch
      val cond = Cond(branch, b)
      touchedCondViews += CondView(cond, getView(cond)) -> getNearest
      super.moveBranch(branch, b)

    // get syntax-sensitive views
    private def getView(node: Node | Cond): View =
      val stack = st.context.featureStack.take(kFs)
      val path = if (cp) then Some(st.context.callPath) else None
      stack match {
        case Nil                  => None
        case feature :: enclosing => Some(enclosing, feature, path)
      }

    // get location information
    private def getNearest: Option[Nearest] = st.context.nearest
  }

  /** meta-information for each script */
  case class ScriptInfo(
    // TODO test: ConformTest,
    touchedNodeViews: Iterable[NodeView],
    touchedCondViews: Iterable[CondView],
  )

  /** syntax-sensitive view */
  type View = Option[(List[Feature], Feature, Option[CallPath])]
  private def stringOfView(view: View) = view.fold("") {
    case (enclosing, feature, path) =>
      s"@ $feature${enclosing.mkString("[", ", ", "]")}:${path.getOrElse("")}"
  }
  sealed trait NodeOrCondView(view: View) {}
  case class NodeView(node: Node, view: View) extends NodeOrCondView(view) {
    override def toString: String = node.simpleString + stringOfView(view)
  }

  case class CondView(cond: Cond, view: View) extends NodeOrCondView(view) {
    override def toString: String = cond.toString + stringOfView(view)
    def neg: CondView = copy(cond = cond.neg)
  }

  case class FuncView(func: Func, view: View) {
    override def toString: String = func.name + stringOfView(view)
  }

  // branch or reference to EReturnIfAbrupt with boolean values
  // `true` (`false`) denotes then- (else-) branch or abrupt (non-abrupt) value
  case class Cond(branch: Branch, cond: Boolean) {
    def neg: Cond = copy(cond = !cond)

    // get id
    inline def id: Int = branch.id

    // condition string
    inline def condString: String = if (cond) "T" else "F"

    // get loc
    inline def loc: Option[Loc] = branch.loc

    // string representation
    def simpleString: String = s"${branch.simpleString}:$condString"

    // conversion to string
    override def toString: String = simpleString
  }

  import scala.math.Ordering.Implicits.seqOrdering

  /** ordering of syntax-sensitive views */
  given Ordering[Feature] = Ordering.by(_.toString)
  given Ordering[CallPath] = Ordering.by(_.toString)
  given Ordering[Node] = Ordering.by(_.id)
  given Ordering[NodeView] = Ordering.by(v => (v.node, v.view))
  given Ordering[Cond] = Ordering.by(cond => cond.id)
  given Ordering[CondView] = Ordering.by(v => (v.cond, v.view))

  // meta-info for each view or features
  case class NodeViewInfo(index: Int, nodeView: NodeView, script: String)
  case class CondViewInfo(index: Int, condView: CondView, script: String)

  case class CoverageConstructor(
    kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
  )
}
