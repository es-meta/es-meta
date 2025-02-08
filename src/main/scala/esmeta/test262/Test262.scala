package esmeta.test262

import esmeta.*
import esmeta.cfg.CFG
import esmeta.error.{NotSupported, InvalidExit, UnexpectedParseResult}
import esmeta.error.NotSupported.*
import esmeta.es.*
import esmeta.es.util.*
import esmeta.es.util.fuzzer.*
import esmeta.interpreter.Interpreter
import esmeta.parser.ESParser
import esmeta.state.*
import esmeta.test262.util.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.{ConcurrentPolicy => CP}
import esmeta.util.SystemUtils.*
import java.util.concurrent.TimeoutException

/** data in Test262 */
case class Test262(
  version: Test262.Version,
  cfg: CFG,
  withYet: Boolean = false,
) {

  /** cache for parsing results for necessary harness files */
  lazy val getHarness =
    cached[String, List[Ast]](name =>
      parseFile(s"$TEST262_DIR/harness/$name").flattenStmt,
    )

  /** test262 filter */
  lazy val testFilter: TestFilter = TestFilter(cfg.spec)

  /** all Test262 tests */
  lazy val allTests: List[Test] = Test.fromDir(TEST262_TEST_DIR)

  /** test262 test configuration */
  lazy val (allTargetTests, allRemoved) = testFilter(allTests, withYet)

  /** basic harness files */
  lazy val basicHarness = getHarness("assert.js") ++ getHarness("sta.js")

  /** specification */
  val spec = cfg.spec

  /** load test262 */
  def loadTest(filename: String): Ast =
    loadTest(filename, Test(filename).includes)

  /** load test262 with harness files */
  def loadTest(filename: String, includes: List[String]): Ast =
    // load harness
    val harnessStmts = includes.foldLeft(basicHarness)(_ ++ getHarness(_))
    // merge with harnesses
    val stmts = parseFile(filename).flattenStmt
    mergeStmt(harnessStmts ++ stmts)

  /** get tests */
  def getTests(
    paths: Option[List[String]] = None,
    features: Option[List[String]] = None,
    log: Boolean = false,
  ): List[Test] =
    if (log) println("- Extracting tests of Test262 tests...")
    Test.fromDirs(paths.getOrElse(List(TEST262_TEST_DIR)), features)

  /** get tests */
  def getProgressBar(
    name: String,
    targetTests: List[Test],
    removed: Iterable[(Test, ReasonPath)] = Nil,
    useProgress: Boolean = false,
    useErrorHandler: Boolean = true,
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
  ): ProgressBar[Test] = ProgressBar(
    msg = s"Run Test262 $name tests",
    iterable = targetTests,
    notSupported = removed,
    getName = (test, _) => test.relName,
    errorHandler = (e, summary, name) =>
      if (useErrorHandler) e match
        case NotSupported(reasons) =>
          summary.notSupported.add(name, reasons)
        case _: TimeoutException =>
          if (verbose) println(s"[TIMEOUT] $name")
          summary.timeout.add(name)
        case e: Throwable =>
          if (verbose) println(s"[FAIL   ] $name")
          summary.fail.add(name, getMessage(e))
      else throw e,
    verbose = useProgress,
    concurrent = concurrent,
  )

  /** interpreter test */
  def evalTest(
    paths: Option[List[String]] = None,
    features: Option[List[String]] = None,
    log: Boolean = false,
    useProgress: Boolean = false,
    useCoverage: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
  ): Summary = {
    // extract tests from paths
    val tests: List[Test] = getTests(paths, features)

    // use error handler for multiple targets
    val multiple = tests.length > 1

    // get target tests and removed tests
    val (targetTests, removed) = testFilter(tests, withYet)

    // get progress bar for extracted tests
    val progressBar = getProgressBar(
      name = "eval",
      targetTests = targetTests,
      removed = removed,
      useProgress = useProgress,
      useErrorHandler = multiple,
      concurrent = concurrent,
      verbose = verbose,
    )

    // coverage with time limit
    lazy val cov = Coverage(
      cfg = cfg,
      timeLimit = timeLimit,
      targetFeatureSetConfig = TargetFeatureSetConfig(),
    )

    // run tests with logging
    logForTests(
      name = "eval",
      progressBar = progressBar,
      postSummary = if (useCoverage) cov.toString else "",
      log = log && multiple,
    )(
      // check final execution status of each Test262 test
      check = test =>
        val filename = test.path
        val st =
          if (!useCoverage) evalFile(filename, log && !multiple, timeLimit)
          else cov.runAndCheck(loadTest(filename), filename)._1
        val returnValue = st(GLOBAL_RESULT)
        if (returnValue != Undef) throw InvalidExit(returnValue)
      ,
      // dump coverage
      postJob = logDir => if (useCoverage) cov.dumpTo(logDir),
    )

    progressBar.summary
  }

  /** parse test */
  def parseTest(
    paths: Option[List[String]] = None,
    log: Boolean = false,
    useProgress: Boolean = false,
    timeLimit: Option[Int] = None, // default: no limit
    concurrent: CP = CP.Single,
    verbose: Boolean = false,
  ): Summary = {
    // extract tests from paths
    val tests: List[Test] = getTests(paths)

    // get target tests and removed tests
    val (targetTests, removed) = testFilter(tests, withYet)

    // get progress bar for extracted tests
    val progressBar = getProgressBar(
      name = "parse",
      targetTests = targetTests,
      removed = removed,
      useProgress = useProgress,
      concurrent = concurrent,
      verbose = verbose,
    )

    // run tests with logging
    logForTests(
      name = "parse",
      progressBar = progressBar,
      log = log,
    )(
      // check parsing result with its corresponding code
      check = test =>
        val filename = test.path
        val ast = parseFile(filename)
        val newAst = parse(ast.toString(grammar = Some(cfg.grammar)))
        if (ast != newAst) throw UnexpectedParseResult,
    )

    progressBar.summary
  }

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // parse ECMAScript code
  private lazy val scriptParser = cfg.scriptParser
  private def parse(code: String): Ast = scriptParser.from(code)
  private def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

  // eval ECMAScript code
  private def evalFile(
    filename: String,
    log: Boolean = false,
    timeLimit: Option[Int] = None,
  ): State = eval(loadTest(filename), log, timeLimit)
  private def eval(
    ast: Ast,
    log: Boolean = false,
    timeLimit: Option[Int] = None,
  ): State =
    val st = cfg.init.from(ast)
    Interpreter(
      st = st,
      log = log,
      logDir = TEST262TEST_LOG_DIR,
      timeLimit = timeLimit,
    )

  // logging mode for tests
  private def logForTests(
    name: String,
    progressBar: ProgressBar[Test],
    postSummary: => String = "",
    log: Boolean = false,
  )(
    check: Test => Unit,
    postJob: String => Unit = _ => {},
  ): Unit =
    val summary: Summary = progressBar.summary
    val logDir = s"$TEST262TEST_LOG_DIR/$name-$dateStr"

    // setting for logging
    if (log)
      mkdir(logDir)
      dumpFile(spec.versionString, s"$logDir/ecma262-version")
      dumpFile(ESMeta.currentVersion, s"$logDir/esmeta-version")

    // run tests
    for (test <- progressBar) check(test)

    // logging after tests
    if (log)
      summary.dumpTo(logDir)
      val summaryStr =
        if (postSummary.isEmpty) s"$summary"
        else s"$summary$LINE_SEP$postSummary"
      dumpFile(s"Test262 $name test summary", summaryStr, s"$logDir/summary")

    // post job
    postJob(logDir)
}
object Test262 extends Git(TEST262_DIR)
