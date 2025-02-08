package esmeta.es.util.fuzzer

import esmeta.cfg.*
import esmeta.spec.*
import esmeta.error.*
import esmeta.mutator.*
import esmeta.synthesizer.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.js.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{ESMeta, FUZZ_LOG_DIR, LINE_SEP}
import io.circe.*, io.circe.syntax.*
import java.io.PrintWriter
import java.util.concurrent.TimeoutException
import scala.collection.mutable.{ListBuffer, Map => MMap}
import scala.collection.parallel.CollectionConverters._
import scala.util.*

import esmeta.phase.MinifyFuzz

/** ECMAScript program fuzzer with ECMA-262 */
class Fuzzer(
  cfg: CFG,
  logInterval: Option[Int] = Some(600), // default is 10 minutes.
  debug: Int = NO_DEBUG, // 2: all, 1: partial, 0: no
  stdOut: Boolean = false,
  timeLimit: Option[Int] = None, // time limitation for each evaluation
  trial: Option[Int] = None, // `None` denotes no bound
  duration: Option[Int] = None, // `None` denotes no bound
  init: Option[String] = None,
  cp: Boolean = false,
  targetFeatSetConfig: TargetFeatureSetConfig,
  minifyCmd: Option[String] = None,
) {
  import Fuzzer.*

  /** ECMAScript grammar */
  lazy val grammar = cfg.grammar
  lazy val scriptParser = cfg.scriptParser

  val kFs = targetFeatSetConfig.maxSensitivity
  val doCleanup = targetFeatSetConfig.doCleanup

  /** generated ECMAScript programs */
  lazy val result: Coverage =
    logInterval.map(_ => {
      // start logging
      mkdir(logDir, remove = true)
      createSymlink(symlink, logDir, overwrite = true)
      dumpFile(ESMeta.currentVersion, s"$logDir/version")
      dumpFile(getSeed, s"$logDir/seed")
      dumpFile(minifyCmd.getOrElse(""), s"$logDir/minifier")
      // TODO(@hyp3rflow): impl JSEngine to check current default engine
      // dumpFile(JSEngine.defaultEngineToString, s"$logDir/default-engine")
      genSummaryHeader
      genStatHeader(selector.names, selStatTsv)
      genStatHeader(mutator.names, mutStatTsv)
    })
    time(
      s"- initializing program pool with ${initPool.size} programs", {
        var i = 1
        for {
          (synthesizer, rawCode) <- initPool
          code <- optional(
            scriptParser.from(rawCode).toString(grammar = Some(grammar)),
          )
        } {
          debugging(f"[${synthesizer}:$i/${initPool.size}%-30s] $code")
          i += 1
          add(code)
        }
      },
    )
    println(s"- the initial program pool consists of ${pool.size} programs.")
    time(
      "- repeatedly trying to fuzz new programs to increase coverage", {
        logInterval.foreach(_ => {
          startTime = System.currentTimeMillis
          startInterval = System.currentTimeMillis
          logging
        })
        trial match
          case Some(count) => for (_ <- Range(0, count)) if (!timeout) fuzz
          case None        => while (!timeout) fuzz
      },
    )

    // finish logging
    logInterval.foreach(_ => {
      logging
      summaryTsv.close
      selStatTsv.close
      mutStatTsv.close
    })

    cov

  /** current program pool */
  def pool: Set[Script] = cov.minimalScripts

  /** one trial to fuzz new programs to increase coverage */
  def fuzz: Unit =
    iter += 1

    debugging(("-" * 40) + f"  iter: $iter%10d  " + ("-" * 40))
    for (bound <- logInterval) {
      val seconds = bound * 1000
      if (interval > seconds) {
        if (debug == NO_DEBUG) logging else time("Logging", logging)
        startInterval += seconds
      }
    }
    val (selectorName, script, condView) = selector(pool, cov)
    val selectorInfo = selectorName + condView.map(" - " + _).getOrElse("")
    val code = script.code
    debugging(f"[$selectorInfo%-30s] $code")
    debugFlush

    val mutants = mutator(code, 100, condView.map((_, cov)))
      .map((name, ast) => (name, ast.toString(grammar = Some(grammar))))
      .distinctBy(_._2)
      .toArray
      .par
      .map(infoExtractor)
      .toList

    for ((mutatorName, mutatedCode, info) <- mutants)
      debugging(f"----- $mutatorName%-20s-----> $mutatedCode")

      val result = add(mutatedCode, info)
      update(selectorName, selectorStat, result)
      update(mutatorName, mutatorStat, result)

    val tmp = System.currentTimeMillis() - startTime
    val duration = Time(tmp)
    debugging(s"iter/end: $iter - $duration")

  /** Case class to hold the information about a candidate */
  case class CandInfo(
    visited: Boolean = false,
    invalid: Boolean = false,
    interp: Option[Coverage.Interp] = None,
  )

  /** Extract information for the mutated code. Should be side-effect free. */
  def infoExtractor(
    mutatorName: String,
    mutatedCode: String,
  ): (String, String, CandInfo) =
    (mutatorName, mutatedCode, getCandInfo(mutatedCode))

  def getCandInfo(code: String): CandInfo =
    if (visited contains code)
      CandInfo(visited = true)
    else if (!ValidityChecker(code))
      CandInfo(invalid = true)
    else
      CandInfo(interp = optional(cov.run(code)))

  /** add new program */
  def add(code: String): Boolean = add(code, getCandInfo(code))

  /** add new program with precomputed info */
  def add(code: String, info: CandInfo): Boolean = handleResult(Try {
    if (info.visited)
      fail("ALREADY VISITED")
    visited += code
    if (info.invalid)
      fail("INVALID PROGRAM")
    val script = toScript(code)
    val interp = info.interp.getOrElse(fail("Interp Fail"))
    val finalState = interp.result
    val (_, updated, covered) = cov.check(script, interp)
    if (!updated) fail("NO UPDATE")
    covered
  })

  /** handle add result */
  def handleResult(result: Try[Boolean]): Boolean =
    debugging(f" ${"COVERAGE RESULT"}%30s: ", newline = false)
    val pass = result match
      case Success(covered)             => debugging(passMsg("")); covered
      case Failure(e: TimeoutException) => debugging(failMsg("TIMEOUT")); false
      case Failure(e: NotSupported) =>
        debugging(failMsg("NOT SUPPORTED")); false
      // handling HostLoadImportedModule AO in ImportCall
      case Failure(e: UnknownFunc) =>
        debugging(failMsg(s"UNKNOWN FUNC (${e.name})")); false
      case Failure(e: ESMetaError) => throw e
      case Failure(e) =>
        e.getMessage match
          case "ALREADY VISITED" | "INVALID PROGRAM" if debug == PARTIAL =>
            debugClean
          case msg =>
            debugging(failMsg(msg))
        false
    debugFlush
    pass

  // a pass-or-fail counter
  case class Counter(pass: Int = 0, fail: Int = 0)
  def update[T](t: T, map: MMap[T, Counter], pass: Boolean): Unit =
    val Counter(p, f) = map.getOrElse(t, Counter())
    val updated = if (pass) Counter(p + 1, f) else Counter(p, f + 1)
    map += t -> updated
  private def counterJson[T: Ordering](map: MMap[T, Counter]): Json =
    JsonObject(
      (for ((condView, Counter(pass, fail)) <- map.toList.sortBy(_._1)) yield {
        val key = condView.toString
        val obj = JsonObject(
          "pass" -> pass.asJson,
          "fail" -> fail.asJson,
        ).asJson
        key -> obj
      }): _*,
    ).asJson

  /** coverage */
  val cov: Coverage =
    Coverage(
      cfg = cfg,
      cp = cp,
      timeLimit = timeLimit,
      logDir = Some(logDir),
      targetFeatureSetConfig = targetFeatSetConfig,
      minifyCmd = minifyCmd,
    )

  /** target selector */
  val selector: TargetSelector = WeightedSelector(
    RandomSelector -> 2,
    BranchSelector -> 8,
  )

  /** selector stat */
  val selectorStat: MMap[String, Counter] = MMap()

  /** mutator */
  given CFG = cfg
  val mutator: Mutator = WeightedMutator(
    NearestMutator(),
    RandomMutator(),
    StatementInserter(),
    Remover(),
    SpecStringMutator(),
  )

  /** mutator stat */
  val mutatorStat: MMap[String, Counter] = MMap()

  /** initial pool */
  val initPool = init
    .map(d =>
      listFiles(d).sorted.map(f =>
        "GivenByUser" -> readFile(f.getPath).replace(USE_STRICT, ""),
      ),
    )
    .getOrElse(
      SimpleSynthesizer(grammar).initPool
        .map(SimpleSynthesizer(grammar).name -> _) ++
      BuiltinSynthesizer(cfg.spec.algorithms).initPool
        .map(BuiltinSynthesizer(cfg.spec.algorithms).name -> _),
    )

  lazy val logDir: String = s"$FUZZ_LOG_DIR/fuzz-$dateStr"
  lazy val symlink: String = s"$FUZZ_LOG_DIR/recent"

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // current iteration count
  protected var iter: Int = 0

  // current id
  private var idCounter: Long = 0
  private def nextId: Long = { val id = idCounter; idCounter += 1; id }

  // evaluation start time
  private var startTime: Long = 0L
  private def elapsed: Long = System.currentTimeMillis - startTime
  private def timeout = duration.fold(false)(_ * 1000 < elapsed)
  private var startInterval: Long = 0L
  private def interval: Long = System.currentTimeMillis - startInterval

  // conversion from code string to `Script` object
  protected def toScript(code: String): Script =
    Script(code, s"$nextId.js", elapsed = Some(elapsed), iter = Some(iter))

  // check if the added code is visited
  protected var visited: Set[String] = Set()

  // indicating that add failed
  protected def fail(msg: String) = throw Exception(msg)

  // debugging
  private var debugMsg = ""
  private def debugging(
    msg: String,
    newline: Boolean = true,
  ): Unit = if (debug == ALL) {
    if (newline) println(msg) else print(msg)
  } else if (debug > NO_DEBUG) {
    debugMsg += msg
    if (newline) debugMsg += LINE_SEP
  }
  private def debugClean: Unit = debugMsg = ""
  private def debugFlush: Unit = { print(debugMsg); debugClean }

  // generate headers
  private def genSummaryHeader =
    var header = Vector(
      "iter(#)",
      "time(ms)",
      "time(h:m:s)",
      "program(#)",
      "minimal(#)",
      "node(#)",
      "branch(#)",
      "minifiable(%)",
    )
    header ++= Vector("target-feat(#)")
    if (kFs > 0) header ++= Vector(s"sens-node(#)", s"sens-branch(#)")
    header ++= Vector("target-conds(#)")
    if (kFs > 0) header ++= Vector(s"sens-target-conds(#)")
    addRow(header)
  private def genStatHeader(keys: List[String], nf: PrintWriter) =
    var header1 = Vector("iter(#)")
    var header2 = Vector("-")
    keys.foreach(k => {
      header1 ++= Vector(k, "-", "-", "-")
      header2 ++= Vector("pass", "fail", "total", "ratio")
    })
    addRow(header1, nf)
    addRow(header2, nf)

  // dump selector and mutator stat
  private def dumpStat(
    keys: List[String],
    stat: MMap[String, Counter],
    tsv: PrintWriter,
  ): Unit =
    var row = Vector[Any](iter)
    keys.foreach(k => {
      val Counter(pass, fail) = stat.getOrElse(k, Counter())
      val total = pass + fail
      val ratio = optional((pass * 10000) / total / 100.0).getOrElse(0.0)
      row ++= Vector(pass, fail, total, s"$ratio%")
    })
    addRow(row, tsv)

  // logging
  def logging: Unit =

    val startTime = System.currentTimeMillis

    val n = cov.nodeCov
    val b = cov.branchCov
    val e = elapsed
    val t = Time(e).simpleString
    val nv = cov.nodeViewCov
    val bv = cov.branchViewCov
    val tc = cov.targetCondViews.size
    val tcv = cov.targetCondViews.map(_._2.size).fold(0)(_ + _)
    val mr = (cov.minifiableRate * 100 * 1000).round / 1000.0
    var row = Vector(iter, e, t, visited.size, pool.size, n, b, mr)
    val targetFeatureSize = cov.targetFeatSet.targetFeatureSize
    row ++= Vector(targetFeatureSize)
    if (kFs > 0) row ++= Vector(nv, bv)
    row ++= Vector(tc)
    if (kFs > 0) row ++= Vector(tcv)
    addRow(row)
    // dump coverage
    cov.dumpToWithDetail(logDir, withMsg = (debug == ALL), silent = true)
    dumpStat(mutator.names, mutatorStat, mutStatTsv)

    val duration = Time(System.currentTimeMillis - startTime)
    println(f"- Logging time: $duration")

  private def addRow(data: Iterable[Any], nf: PrintWriter = summaryTsv): Unit =
    val row = data.mkString("\t")
    if (stdOut) println(row)
    nf.println(row)
    nf.flush
  private lazy val summaryTsv: PrintWriter = getPrintWriter(
    s"$logDir/summary.tsv",
  )
  private lazy val selStatTsv: PrintWriter = getPrintWriter(
    s"$logDir/selector-stat.tsv",
  )
  private lazy val mutStatTsv: PrintWriter = getPrintWriter(
    s"$logDir/mutation-stat.tsv",
  )

  def getElapsed: Long = elapsed
}

object Fuzzer {
  // debugging levels
  val ALL = 2
  val PARTIAL = 1
  val NO_DEBUG = 0
}
