package esmeta.es.util

import esmeta.LINE_SEP
import esmeta.cfg.*
import esmeta.injector.*
import esmeta.interpreter.*
import esmeta.ir.{EReturnIfAbrupt, Expr, EParse, EBool}
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.Coverage.Interp
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.util.fuzzer.{MinifyChecker, FSTreeWrapper, FSTreeConfig}
import esmeta.js.minifier.Minifier
import esmeta.util.BaseUtils.chiSqDistTable
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import scala.collection.mutable.{Map => MMap}

import scala.math.Ordering.Implicits.seqOrdering
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import esmeta.phase.MinifyFuzz

/** coverage measurement of cfg */
case class Coverage(
  cfg: CFG,
  cp: Boolean = false,
  timeLimit: Option[Int] = None,
  logDir: Option[String] = None, // TODO: use this
  fsTreeConfig: FSTreeConfig,
  minifyCmd: Option[String] = None,
) {
  import Coverage.{*, given}

  val jsonProtocol: JsonProtocol = JsonProtocol(cfg)
  import jsonProtocol.given

  val kFs = fsTreeConfig.maxSensitivity
  val isSelective = fsTreeConfig.isSelective
  val useSrv = fsTreeConfig.useSrv
  val fsTrie = new FSTreeWrapper(config = fsTreeConfig)

  // minimal scripts
  def minimalScripts: Set[Script] = _minimalScripts
  private var _minimalScripts: Set[Script] = Set()

  // meta-info of each script
  private var _minimalInfo: Map[String, ScriptInfo] = Map()
  def minifiableRate: Double = _minimalInfo.values.count(
    _.minifiable.getOrElse(false),
  ) / _minimalInfo.size.toDouble

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
  def runAndCheck(script: Script): (State, Boolean, Boolean) = {
    val interp = run(script.code)
    this.synchronized(check(script, interp))
  }

  def runAndCheck(ast: Ast, name: String): (State, Boolean, Boolean) = {
    val code = ast.toString(grammar = Some(cfg.grammar))
    val interp = run(code)
    this.synchronized(check(Script(code, name), interp))
  }

  def runAndCheckWithBlocking(
    script: Script,
    modify: Boolean = true,
  ): (State, Boolean, Boolean, Set[Script], Set[NodeView], Set[CondView]) = {
    val interp = run(script.code)
    this.synchronized(checkWithBlockings(script, interp, modify))
  }

  /** evaluate a given ECMAScript program */
  def run(code: String): Interp = {
    val initSt = cfg.init.from(code)
    val interp = Interp(initSt, cp, timeLimit)
    interp.result; interp
  }

  /** evaluate a given ECMAScript AST */
  def run(ast: Ast): Interp = {
    val initSt = cfg.init.from(ast)
    val interp = Interp(initSt, cp, timeLimit)
    interp.result; interp
  }

  def check(script: Script, interp: Interp): (State, Boolean, Boolean) =
    val Script(code, _, _, _, _) = script
    val initSt = cfg.init.from(code)
    val finalSt = interp.result

    var covered = false
    var updated = false
    var blockingScripts: Set[Script] = Set.empty

    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    val strictCode = USE_STRICT + code

    val isMinifierHit = Minifier.checkMinifyDiff(strictCode, minifyCmd)

    val rawStacks =
      interp.touchedNodeViews.keys
        .flatMap(_.view)
        .map(v => (v._2 :: v._1).map(_.func.name))

    if (isMinifierHit) then fsTrie.touchWithHit(rawStacks)
    else fsTrie.touchWithMiss(rawStacks)

    // update node coverage
    for ((rawNodeView, nearest) <- interp.touchedNodeViews)
      // cut out features TODO: do this in the interpreter (Kanguk Lee)
      val NodeView(node, rawView) = rawNodeView
      val view: View = rawView.flatMap {
        case (rawEnclosing, feature, path) =>
          val rawStack = feature :: rawEnclosing
          val featureStack = rawStack.take(fsTrie((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val nodeView = NodeView(node, view)
      touchedNodeViews += nodeView -> nearest
      getScript(nodeView) match
        case None => update(nodeView, script); updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
          blockingScripts += originalScript
        case Some(blockScript) => blockingScripts += blockScript

    // update branch coverage
    for ((rawCondView, nearest) <- interp.touchedCondViews)
      // cut out features TODO: do this in the interpreter (Kanguk Lee)
      val CondView(cond, rawView) = rawCondView
      val view: View = rawView.flatMap {
        case (rawEnclosing, feature, path) =>
          val rawStack = feature :: rawEnclosing
          val featureStack = rawStack.take(fsTrie((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val condView: CondView = CondView(cond, view)
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
        ConformTest.createTest(cfg, finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
        minifiable = Some(isMinifierHit),
      )

    // TODO: impl checkWithBlocking using `blockingScripts`
    (finalSt, updated, covered)

  // check with both blocking and kicked scripts
  // NOTE: MinifyFuzzer uses it
  def checkWithTree(
    script: Script,
    interp: Interp,
  ): (
    State,
    Boolean,
    Boolean,
  ) =
    val Script(code, _, _, _, _) = script
    val strictCode = USE_STRICT + code
    val isMinifierHitOptFuture = Future {
      if (isSelective)
        if (useSrv)
          Some(Minifier.checkMinifyDiffSrv(strictCode, minifyCmd))
        else
          Some(Minifier.checkMinifyDiff(strictCode, minifyCmd))
      else None
    }

    val initSt = cfg.init.from(code)
    val finalSt = interp.result

    var covered = false
    var updated = false

    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    val rawStacks =
      interp.touchedNodeViews.keys
        .flatMap(_.view)
        .map(v => (v._2 :: v._1).map(_.func.name))

    // update node coverage
    for ((rawNodeView, nearest) <- interp.touchedNodeViews)
      // cut out features TODO: do this in the interpreter (Kanguk Lee)
      val NodeView(node, rawView) = rawNodeView
      val view: View = rawView.flatMap {
        case (rawEnclosing, feature, path) =>
          val rawStack = feature :: rawEnclosing
          val featureStack = rawStack.take(fsTrie((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val nodeView = NodeView(node, view)
      touchedNodeViews += nodeView -> nearest
      getScript(nodeView) match
        case None => update(nodeView, script); updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          update(nodeView, script)
          updated = true
        case Some(blockScript) => ()

    // update branch coverage
    for ((rawCondView, nearest) <- interp.touchedCondViews)
      // cut out features TODO: do this in the interpreter (Kanguk Lee)
      val CondView(cond, rawView) = rawCondView
      val view: View = rawView.flatMap {
        case (rawEnclosing, feature, path) =>
          val rawStack = feature :: rawEnclosing
          val featureStack = rawStack.take(fsTrie((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val condView: CondView = CondView(cond, view)
      touchedCondViews += condView -> nearest
      getScript(condView) match
        case None =>
          update(condView, nearest, script); updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          update(condView, nearest, script)
          updated = true
        case Some(blockScript) => ()

    val isMinifierHitOpt = Await.result(isMinifierHitOptFuture, Duration.Inf)

    isMinifierHitOpt match
      case Some(true)  => fsTrie.touchWithHit(rawStacks)
      case Some(false) => fsTrie.touchWithMiss(rawStacks)
      case _           => /* do nothing */
    if (updated)
      _minimalInfo += script.name -> ScriptInfo(
        ConformTest.createTest(cfg, finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
        minifiable = isMinifierHitOpt,
      )

    // TODO: impl checkWithBlocking using `blockingScripts`
    (finalSt, updated, covered)

  // not updating fstrie
  def checkWithBlockings(
    script: Script,
    interp: Interp,
    modify: Boolean,
  ): (State, Boolean, Boolean, Set[Script], Set[NodeView], Set[CondView]) =
    val Script(code, _, _, _, _) = script
    val initSt = cfg.init.from(code)
    val finalSt = interp.result

    var covered = false
    var updated = false
    var blockingScripts: Set[Script] = Set.empty
    var coveredNodeViews = Set.empty[NodeView]
    var coveredCondViews = Set.empty[CondView]

    var touchedNodeViews: Map[NodeView, Option[Nearest]] = Map()
    var touchedCondViews: Map[CondView, Option[Nearest]] = Map()

    val rawStacks =
      interp.touchedNodeViews.keys
        .flatMap(_.view)
        .map(v => (v._2 :: v._1).map(_.func.name))

    // update node coverage
    for ((rawNodeView, nearest) <- interp.touchedNodeViews)
      // cut out features TODO: do this in the interpreter (Kanguk Lee)
      val NodeView(node, rawView) = rawNodeView
      val view: View = rawView.flatMap {
        case (rawEnclosing, feature, path) =>
          val rawStack = feature :: rawEnclosing
          val featureStack = rawStack.take(fsTrie((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val nodeView = NodeView(node, view)
      touchedNodeViews += nodeView -> nearest
      getScript(nodeView) match
        case None =>
          if modify then update(nodeView, script)
          coveredNodeViews += nodeView;
          updated = true; covered = true
        case Some(originalScript) if originalScript.code.length > code.length =>
          if modify then update(nodeView, script)
          updated = true
          blockingScripts += originalScript
        case Some(blockScript) => blockingScripts += blockScript

    // update branch coverage
    for ((rawCondView, nearest) <- interp.touchedCondViews)
      // cut out features TODO: do this in the interpreter (Kanguk Lee)
      val CondView(cond, rawView) = rawCondView
      val view: View = rawView.flatMap {
        case (rawEnclosing, feature, path) =>
          val rawStack = feature :: rawEnclosing
          val featureStack = rawStack.take(fsTrie((rawStack).map(_.func.name)))
          if featureStack.isEmpty then None
          else Some((featureStack.tail, featureStack.head, path))
      }
      val condView: CondView = CondView(cond, view)
      touchedCondViews += condView -> nearest
      getScript(condView) match
        case None =>
          if modify then update(condView, nearest, script)
          coveredCondViews += condView;
          updated = true; covered = true
        case Some(origScript) if origScript.code.length > code.length =>
          if modify then update(condView, nearest, script)
          updated = true
          blockingScripts += origScript
        case Some(blockScript) => blockingScripts += blockScript

    if (updated)
      _minimalInfo += script.name -> ScriptInfo(
        ConformTest.createTest(cfg, finalSt),
        touchedNodeViews.keys,
        touchedCondViews.keys,
      )

    (
      finalSt,
      updated,
      covered,
      blockingScripts,
      coveredNodeViews,
      coveredCondViews,
    )

  def cleanup(): Unit = {
    val mortalNodeViews = nodeViews.filterNot {
      case NodeView(_, Some((tail, head, _))) =>
        val stack = head :: tail
        val sensitiveLength = fsTrie(stack.map(_.func.name))
        sensitiveLength >= stack.length
      case _ => true
    }
    val mortalCondViews = condViews.filterNot {
      case CondView(cond, Some((tail, head, _))) =>
        val stack = head :: tail
        val sensitiveLength = fsTrie(stack.map(_.func.name))
        sensitiveLength >= stack.length
      case _ => true
    }

    val woundScripts = mortalNodeViews.map(getScript).flatten ++
      mortalCondViews.map(getScript).flatten

    for (nodeView <- mortalNodeViews) {
      val NodeView(node, view) = nodeView
      nodeViewMap += node -> (apply(node) - view)
    }

    for (condView <- mortalCondViews) {
      val CondView(cond, view) = condView
      condViewMap += cond -> (apply(cond) - view)
      removeTargetCond(condView)
    }

    nodeViews --= mortalNodeViews
    condViews --= mortalCondViews

    for (script <- woundScripts) {
      val count = counter(script) - 1
      counter += (script -> count)
      if (count == 0) {
        counter -= script
        _minimalScripts -= script
        _minimalInfo -= script.name
      }
    }

    println(
      s"cleanup: ${mortalNodeViews.size} node views, ${mortalCondViews.size} cond views, ${woundScripts.size} scripts",
    )

  }

  /** get node coverage */
  def nodeCov: Int = nodeViewMap.size
  def nodeViewCov: Int = nodeViews.size

  /** get branch coverage */
  def branchCov: Int = condViewMap.size
  def branchViewCov: Int = condViews.size

  /** dump results with detail */
  def dumpToWithDetail(
    baseDir: String,
    withMsg: Boolean = true,
    silent: Boolean = false,
  ): Unit = dumpTo(
    baseDir = baseDir,
    withScripts = true,
    withScriptInfo = true,
    withTargetCondViews = true,
    withUnreachableFuncs = true,
    withFStrie = true,
    withMsg = withMsg,
    silent = silent,
  )

  /** dump results */
  def dumpTo(
    baseDir: String,
    withScripts: Boolean = false,
    withScriptInfo: Boolean = false,
    withTargetCondViews: Boolean = false,
    withUnreachableFuncs: Boolean = false,
    withFStrie: Boolean = false,
    // TODO(@hyp3rflow): use this for ignoring dump messages
    withMsg: Boolean = false,
    silent: Boolean = false,
  ): Unit =
    mkdir(baseDir)
    lazy val orderedNodeViews = nodeViews.toList.sorted
    lazy val orderedCondViews = condViews.toList.sorted
    lazy val getNodeViewsId = orderedNodeViews.zipWithIndex.toMap
    lazy val getCondViewsId = orderedCondViews.zipWithIndex.toMap
    dumpJson(
      name = "constructor",
      data = CoverageConstructor(
        fsTreeConfig.maxSensitivity,
        cp,
        timeLimit,
        fsTreeConfig.promotionThreshold,
        fsTreeConfig.demotionThreshold,
      ),
      filename = s"$baseDir/constructor.json",
      noSpace = false,
      silent = silent,
    )

    val st = System.nanoTime()
    def elapsedSec = (System.nanoTime() - st) / 1e9
    def log(msg: Any): Unit = if (withMsg) println(s"[${elapsedSec}s] $msg")

    dumpJsonChunks(
      name = "node coverage",
      iterable = orderedNodeViews,
      filename = s"$baseDir/node-coverage-chunks.json",
      noSpace = false,
      chunkSize = 40000,
      silent = silent,
    )
    log("Dumped node coverage")
    dumpJsonChunks(
      name = "branch coverage",
      iterable = orderedCondViews,
      filename = s"$baseDir/branch-coverage-chunks.json",
      noSpace = false,
      chunkSize = 40000,
      silent = silent,
    )
    log("Dumped branch coverage")
    if (withScripts)
      dumpDir[Script](
        name = "minimal ECMAScript programs",
        iterable = _minimalScripts,
        dirname = s"$baseDir/minimal",
        getName = _.name,
        getData = script =>
          f"// iter: ${script.iter.getOrElse(-1)}%d, elapsed: ${script.elapsed
            .getOrElse(-1L)}%d ms" +
          LINE_SEP + USE_STRICT + script.code + LINE_SEP,
        remove = true,
        silent = silent,
      )
      val minifiableMinimalScripts = _minimalScripts.filter(s =>
        _minimalInfo.get(s.name).exists(_.minifiable.getOrElse(false)),
      )
      dumpDir[Script](
        name = "minimal ECMAScript programs (minifiable)",
        iterable = minifiableMinimalScripts,
        dirname = s"$baseDir/minimal-minifiable",
        getName = _.name,
        getData = USE_STRICT + _.code + LINE_SEP,
        remove = true,
        silent = silent,
      )
      dumpDir[Script](
        name = "minimal ECMAScript programs (not minifiable)",
        iterable = _minimalScripts -- minifiableMinimalScripts,
        dirname = s"$baseDir/minimal-not-minifiable",
        getName = _.name,
        getData = USE_STRICT + _.code + LINE_SEP,
        remove = true,
        silent = silent,
      )
      log("Dumped scripts")
    if (withScriptInfo)
      dumpDir[(String, ScriptInfo)](
        name = "minimal ECMAScript assertions",
        iterable = _minimalInfo,
        dirname = s"$baseDir/minimal-assertion",
        getName = _._1,
        getData = _._2.test.core, // TODO: dump this as json?
        remove = true,
        silent = silent,
      )
      log("Dumped assertions")
    if (withTargetCondViews)
      dumpJson(
        name = "target conditional branches",
        data = (for {
          (cond, viewMap) <- _targetCondViews
          (view, _) <- viewMap
        } yield getCondViewsId(CondView(cond, view))).toSeq.sorted.asJson,
        filename = s"$baseDir/target-conds.json",
        noSpace = false,
        silent = silent,
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
        silent = silent,
      )
      log("dumped unreachable functions")
    if (withFStrie)
      import fsTrie.given
      val fsTrieConfig = fsTrie.config
      val fsTrieRoot = fsTrie.root
      dumpJson(
        name = "fstrie config",
        data = fsTrieConfig,
        filename = s"$baseDir/fstrie-config.json",
        noSpace = false,
        silent = silent,
      )
      dumpJson(
        name = "fstrie root",
        data = fsTrieRoot,
        filename = s"$baseDir/fstrie-root.json",
        noSpace = false,
        silent = silent,
      )
      log("dumped fstriewrapper")

  /** conversion to string */
  private def percent(n: Double, t: Double): Double = n / t * 100
  override def toString: String =
    val app = new Appender
    (app >> "- coverage:").wrap("", "") {
      app :> "- node: " >> nodeCov
      app :> "- branch: " >> branchCov
    }
    if (fsTreeConfig.maxSensitivity > 0)
      (app :> "- sensitive coverage:").wrap("", "") {
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
  ): Unit =
    condViews += condView
    val CondView(cond, view) = condView

    // update target branches
    val neg = condView.neg
    cond.elem match
      case _ if nearest.isEmpty                               =>
      case Branch(_, _, EBool(_), _, _)                       =>
      case b: Branch if b.isChildPresentCheck(cfg)            =>
      case ref: WeakUIdRef[EReturnIfAbrupt] if !ref.get.check =>
      case _ if getScript(neg).isDefined => removeTargetCond(neg)
      case _                             => addTargetCond(condView, nearest)

    condViewMap += cond -> updated(apply(cond), view, script)

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
    // kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
  ) extends Interpreter(initSt, timeLimit = timeLimit, keepProvenance = true) {
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

    // override helper for return-if-abrupt cases
    override def returnIfAbrupt(
      riaExpr: EReturnIfAbrupt,
      value: Value,
      check: Boolean,
    ): Value =
      val abrupt = value.isAbruptCompletion
      val cond = Cond(riaExpr.idRef, abrupt)

      touchedCondViews += CondView(cond, getView(cond)) -> getNearest
      super.returnIfAbrupt(riaExpr, value, check)

    // get syntax-sensitive views
    private def getView(node: Node | Cond): View =
      val stack = st.context.featureStack
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
    test: ConformTest,
    touchedNodeViews: Iterable[NodeView],
    touchedCondViews: Iterable[CondView],
    minifiable: Option[Boolean] = None,
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
  case class Cond(elem: Branch | WeakUIdRef[EReturnIfAbrupt], cond: Boolean) {
    def neg: Cond = copy(cond = !cond)

    // short kind string
    def kindString: String = elem match
      case (branch: Branch)     => "Branch"
      case (ref: WeakUIdRef[_]) => "EReturnIfAbrupt"

    def shortKindString: String = kindString.take(1)

    // get id
    def id: Int = elem match
      case (branch: Branch)     => branch.id
      case (ref: WeakUIdRef[_]) => ref.id

    // condition string
    def condString: String = if (cond) "T" else "F"

    // get node
    def node: Option[Node] = elem match
      case branch: Branch                   => Some(branch)
      case ref: WeakUIdRef[EReturnIfAbrupt] => ref.get.cfgNode

    // get loc
    def loc: Option[Loc] = elem match
      case branch: Branch                   => branch.loc
      case ref: WeakUIdRef[EReturnIfAbrupt] => ref.get.loc

    // conversion to string
    override def toString: String = s"$kindString[$id]:$condString"

    def simpleString: String = s"$shortKindString[$id]:$condString"
  }

  /** ordering of syntax-sensitive views */
  given Ordering[Feature] = Ordering.by(_.toString)
  given Ordering[CallPath] = Ordering.by(_.toString)
  given Ordering[Node] = Ordering.by(_.id)
  given Ordering[NodeView] = Ordering.by(v => (v.node, v.view))
  given Ordering[Cond] = Ordering.by(cond => (cond.kindString, cond.id))
  given Ordering[CondView] = Ordering.by(v => (v.cond, v.view))

  // meta-info for each view or features
  case class NodeViewInfo(index: Int, nodeView: NodeView, script: String)
  case class CondViewInfo(index: Int, condView: CondView, script: String)

  case class CoverageConstructor(
    kFs: Int,
    cp: Boolean,
    timeLimit: Option[Int],
    proThreshold: Double,
    demThreshold: Double,
  )

  def fromLogSimpl(baseDir: String, cfg: CFG): Coverage =
    val jsonProtocol = JsonProtocol(cfg)
    import jsonProtocol.given
    given fsTrieConfigDecoder: Decoder[FSTreeConfig] = deriveDecoder

    def readJsonHere[T](json: String)(using Decoder[T]) =
      readJson[T](s"$baseDir/$json")

    val con: CoverageConstructor = readJsonHere("constructor.json")
    val fsTreeConfig = readJsonHere[FSTreeConfig]("fstrie-config.json")
    val cov = new Coverage(
      cfg = cfg,
      cp = con.cp,
      timeLimit = con.timeLimit,
      fsTreeConfig = fsTreeConfig,
    )
    cov.fsTrie.replaceRootFromFile(f"$baseDir/fstrie-root.json")
    cov.fsTrie.fixed = true

    val minimalFiles = listFiles(s"$baseDir/minimal")
    println(
      s"coverage initialized. loading ${minimalFiles.size} minimal scripts",
    )

    for (
      minimal <- ProgressBar(
        "reconstructing coverage",
        minimalFiles,
        getName = (x, _) => x.getName(),
        detail = false,
        concurrent = ConcurrentPolicy.Single,
        timeLimit = Some(100),
      )
    ) {
      val name = minimal.getName
      if jsFilter(name) then
        val code = readFile(minimal.getPath).linesIterator
          .filterNot(_.trim.startsWith("//"))
          .mkString("\n")
          .strip
          .drop(USE_STRICT.length)
          .strip
        try {
          val script = Script(code, name)
          cov.runAndCheckWithBlocking(script)
        } catch {
          case e =>
            println(f"Error in $name%-12s: $e  :  $code")
        }
    }

    cov
}
