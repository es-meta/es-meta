package esmeta

import esmeta.phase.*
import esmeta.util.ArgParser

/** commands
  *
  * @tparam Result
  *   the result typeof command
  */
sealed abstract class Command[Result](
  /** command name */
  val name: String,

  /** phase list */
  val pList: PhaseList[Result],
) {
  override def toString: String = pList.toString

  /** help message */
  def help: String

  /** help message */
  def examples: List[String]

  /** show the final result */
  def showResult(res: Result): Unit = println(res)

  /** target name */
  def targetName: String = ""

  /** need target */
  def needTarget: Boolean = targetName != ""

  /** run command with command-line arguments */
  def apply(args: List[String]): Result =
    val cmdConfig = CommandConfig(this)
    val parser = ArgParser(this, cmdConfig)
    val runner = pList.getRunner(parser)
    parser(args)
    ESMeta(this, runner(_), cmdConfig)

  /** run command with command-line arguments */
  def apply(args: String): Result = apply(args.split(" +").toList)

  /** a list of phases without specific IO types */
  def phases: Vector[Phase[_, _]] = pList.phases

  /** append a phase to create a new phase list */
  def >>[R](phase: Phase[Result, R]): PhaseList[R] = pList >> phase
}

/** base command */
case object CmdBase extends Command("", PhaseNil) {
  val help = "does nothing."
  val examples = Nil
}

/** `help` command */
case object CmdHelp extends Command("help", CmdBase >> Help) {
  val help = "shows help messages."
  val examples = List(
    "esmeta help                  // show help message.",
    "esmeta help extract          // show help message of `extract` command.",
  )
  override val targetName = "[<command>]"
  override val needTarget = false
}

// -----------------------------------------------------------------------------
// Mechanized Specification Extraction
// -----------------------------------------------------------------------------
/** `extract` command */
case object CmdExtract extends Command("extract", CmdBase >> Extract) {
  val help = "extracts specification model from ECMA-262 (spec.html)."
  val examples = List(
    "esmeta extract                           // extract current version.",
    "esmeta extract -extract:target=es2024    // extract es2024 version.",
    "esmeta extract -extract:target=868fe7a   // extract 868fe7a hash version.",
  )
}

/** `compile` command */
case object CmdCompile extends Command("compile", CmdExtract >> Compile) {
  val help = "compiles a specification to an IR program."
  val examples = List(
    "esmeta compile                        # compile spec to IR program.",
    "esmeta compile -extract:target=es2024 # compile es2024 spec to IR program",
  )
}

/** `build-cfg` command */
case object CmdBuildCFG extends Command("build-cfg", CmdCompile >> BuildCFG) {
  val help = "builds a control-flow graph (CFG) from an IR program."
  val examples = List(
    "esmeta build-cfg                          # build CFG for spec.",
    "esmeta build-cfg -extract:target=es2024   # build CFG for es2024 spec.",
  )
}

// -----------------------------------------------------------------------------
// Program Collector
// -----------------------------------------------------------------------------
/** `fuzz` command */
case object CmdFuzz extends Command("fuzz", CmdBuildCFG >> Fuzz) {
  val help = "generate ECMAScript programs for fuzzing."
  val examples = List(
    "esmeta fuzz                 # generate ECMAScript programs for fuzzing",
    "esmeta fuzz -fuzz:log       # fuzz in the logging mode.",
  )
}

/** `test262-test` command */
case object CmdTest262Test
  extends Command("test262-test", CmdBuildCFG >> Test262Test) {
  val help = "tests Test262 tests with harness files (default: tests/test262)."
  val examples = List(
    "esmeta test262-test                                           # all ",
    "esmeta test262-test tests/test262/test/built-ins/Map/map.js   # file",
    "esmeta test262-test tests/test262/test/language/expressions   # directory",
  )
  override val targetName = "<js|dir>+"
  override val needTarget = false
}

/** `construct` command */
case object CmdConstruct
  extends Command("construct", CmdBuildCFG >> Construct) {
  val help = "starts a web server for an ECMAScript double debugger."
  val examples = List(
    "esmeta construct    # builds file for ECMAVisualizer.",
  )
}

// -----------------------------------------------------------------------------
// Double Debugger
// -----------------------------------------------------------------------------
/** `web` command */
case object CmdWeb extends Command("web", CmdBuildCFG >> Web) {
  val help = "starts a web server for an ECMAScript double debugger."
  val examples = List(
    "esmeta web    # turn on the server (Use with `esmeta-debugger-client`).",
  )
}
