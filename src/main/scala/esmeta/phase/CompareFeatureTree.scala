package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.es.util.fuzzer.FSTreeWrapper
import scala.collection.mutable.Map as MMap

case object CompareFeatureTree extends Phase[Unit, Unit] {
  val name = "compare-feature-tree"
  val help = "extract the statistics from a given fstrie json file."

  val babelBugKeyStacks = Set(
    List(
      "ClassStaticBlockBody[0,0]",
    ),
    List(
      "Initializer[0,0]",
      "ClassTail[0,1]",
    ),
    List(
      "AsyncFunctionExpression[0,0]",
    ),
    List(
      "GeneratorExpression[0,0]",
    ),
    List(
      "YieldExpression[2,0]",
    ),
    List(
      "AwaitExpression[0,0]",
      "ElementList[0,0]",
    ),
    List(
      "GeneratorExpression[0,0]",
    ),
    List(
      "ForInOfStatement[8,0]",
    ),
    List(
      "AsyncArrowFunction[0,0]",
    ),
    List(
      "SwitchStatement[0,0]",
    ),
  )

  val babelBugFullStacks = Set(
    List(
      "VariableStatement[0,0]",
      "ClassStaticBlockBody[0,0]",
    ),
    List(
      "IdentifierReference[0,0]",
      "AssignmentExpression[4,0]",
      "Initializer[0,0]",
      "ClassTail[0,1]",
    ),
    List(
      "SuperProperty[0,0]",
      "UnaryExpression[1,0]",
      "Initializer[0,0]",
      "ClassTail[0,1]",
    ),
    List(
      "AsyncFunctionExpression[0,0]",
    ),
    List(
      "GeneratorExpression[0,0]",
    ),
    List(
      "Literal[2,0]",
      "YieldExpression[2,0]",
    ),
    List(
      "AwaitExpression[0,0]",
      "ElementList[0,0]",
      "ArrayLiteral[2,1]",
    ),
    List(
      "GeneratorExpression[0,0]",
    ),
    List(
      "PrimaryExpression[0,0]",
      "UnaryExpression[1,0]",
      "Initializer[0,0]",
      "ClassTail[0,1]",
    ),
    List(
      "VariableStatement[0,0]",
      "ForInOfStatement[8,0]",
    ),
    List(
      "AsyncArrowFunction[0,0]",
      "VariableDeclaration[0,1]",
    ),
    List(
      "NewTarget[0,0]",
      "RelationalExpression[5,0]",
      "Initializer[0,0]",
      "ClassTail[0,1]",
    ),
    List(
      "FunctionDeclaration[0,0]",
      "SwitchStatement[0,0]",
    ),
  )
  def apply(unit: Unit, cmdConfig: CommandConfig, config: Config): Unit =
    val baseDir = getFirstFilename(cmdConfig, "compare-feature-tree")
    val trie = FSTreeWrapper.fromDir(baseDir, fixed = true)
    val featureStacks = trie.stacks
    import trie.given

    val bugStacks = (config.transpiler match {
      case Some("babel") => babelBugFullStacks
      case _             => throw new Exception("Unknown transpiler")
    }).map(_.take(trie.config.maxSensitivity))

    val babelComparison = bugStacks.map { bugStack =>
      val filteredStacks = featureStacks.filter { featStackRaw =>
        (1 to featStackRaw.size).exists { windowSize =>
          featStackRaw.sliding(windowSize).exists { window =>
            if (window.size < bugStack.size) false
            else window.zip(bugStack).forall { case (a, b) => a.startsWith(b) }
          }
        }

      // _.zip(stack).forall((a, b) => a.startsWith(b))
      }
      bugStack -> filteredStacks
    }
    config.out.foreach { path =>
      dumpJson(babelComparison, path)
    }
    ()

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
    (
      "transpiler",
      StrOption((c, s) => c.transpiler = Some(s)),
      "transpiler to use as a bug source.",
    ),
  )

  class Config(
    var transpiler: Option[String] = None,
    var out: Option[String] = None,
  )
}
