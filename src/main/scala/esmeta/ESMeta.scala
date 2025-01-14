package esmeta

import esmeta.error.*
import esmeta.phase.*
import esmeta.util.*
import esmeta.util.BaseUtils.*

/** ESMeta top-level object */
object ESMeta extends Git(BASE_DIR) {

  /** the main entry point of ESMeta. */
  def main(tokens: Array[String]): Unit = try
    tokens.toList match
      case Nil                                        => println(welcome)
      case List("--version" | "-version" | "version") => println(VERSION)
      case str :: args =>
        cmdMap.get(str) match {
          case Some(cmd) => cmd(args)
          case None      => throw NoCmdError(str)
        }
  catch
    // ESMetaError: print only the error message.
    case e: ESMetaError =>
      Console.err.println(getMessage(e))
      if (ERROR_MODE) throw e
      if (STATUS_MODE) sys.exit(1)
    // Unexpected: print the stack trace.
    case e: Throwable =>
      Console.err.println(s"[ESMeta v$VERSION] Unexpected error occurred:")
      throw e

  /** execute ESMeta with a runner */
  def apply[Result](
    command: Command[Result],
    runner: CommandConfig => Result,
    config: CommandConfig,
  ): Result =
    // silent for help command
    if (command == CmdHelp) config.silent = true
    // target existence check
    if (command.needTarget && config.targets.isEmpty)
      throw NoTargetError(command)
    // set the start time.
    val startTime = System.currentTimeMillis
    // execute the command.
    val result: Result = runner(config)
    // duration
    val duration = Time(System.currentTimeMillis - startTime)
    // display the result.
    if (!config.silent) command.showResult(result)
    // display the time.
    if (config.time)
      val name = config.command.name
      println(f"The command '$name' took $duration.")
    // return result
    result

  /** welcome message */
  val welcome: String =
    s"""Welcome to ESMeta v$VERSION - ECMAScript Specification Metalanguage.
       |Please type `esmeta help` to see the help message.""".stripMargin

  /** commands */
  val commands: List[Command[_]] = List(
    CmdHelp,
    // Mechanized Specification Extraction
    CmdExtract,
    CmdCompile,
    CmdBuildCFG,
    // Program Collector
    CmdFuzz,
    CmdTest262Test,
    // Double Debugger
    CmdWeb,
  )
  val cmdMap = commands.foldLeft[Map[String, Command[_]]](Map()) {
    case (map, cmd) => map + (cmd.name -> cmd)
  }

  /** phases */
  var phases: List[Phase[_, _]] = List(
    Help,
    // Mechanized Specification Extraction
    Extract,
    Compile,
    BuildCFG,
    // Program Collector
    Fuzz,
    Test262Test,
    // Double Debugger
    Web,
  )

  /** command options */
  val options: List[PhaseOption[CommandConfig]] = List(
    ("silent", BoolOption(_.silent = _), "do not show final results."),
    ("error", BoolOption((_, b) => ERROR_MODE = b), "show error stack traces."),
    ("status", BoolOption((_, b) => STATUS_MODE = b), "exit with status."),
    ("time", BoolOption(_.time = _), "display the duration time."),
    (
      "test262dir",
      StrOption((c, s) => TEST262_DIR = s),
      "set the directory of Test262 (default: $ESMETA_HOME/tests/test262).",
    ),
  )
}

/** command configuration */
case class CommandConfig(
  var command: Command[_],
  var targets: List[String] = Nil,
  var silent: Boolean = false,
  var time: Boolean = false,
)
