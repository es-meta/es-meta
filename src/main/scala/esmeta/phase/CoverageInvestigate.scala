package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.es.util.USE_STRICT
import esmeta.es.util.fuzzer.{MinifierDB, MinifyChecker}
import esmeta.es.Script
import esmeta.js.minifier.Minifier
import scala.util.*
import java.util.concurrent.atomic.AtomicLong
import esmeta.util.SystemUtils.*
import scala.collection.parallel.CollectionConverters._
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

case object CoverageInvestigate extends Phase[CFG, Unit] {
  val name = "coverage-investigate"
  val help = "investigate newly covered parts by checking additional scripts."

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    import esmeta.ty.util.JsonProtocol.given

    config.checkJson match
      case Some(filename) =>
        given CovInvDataDecoder: Decoder[CovInvData] = deriveDecoder
        given CovInvBugDataEncoder: Encoder[CovInvBugData] = deriveEncoder

        val data = readJson[List[CovInvData]](filename)
        var res = List.empty[CovInvBugData]
        println(s"Checking ${data.size} scripts")
        for (
          covInvData <- ProgressBar(
            "Checking",
            data,
            getName = (x, _) => x.name,
            detail = false,
            concurrent = ConcurrentPolicy.Auto,
          )
        ) {
          val CovInvData(
            name,
            code,
            covered,
            blockings,
            coveredNode,
            coveredBranch,
          ) = covInvData
          val strictCode = USE_STRICT + code
          val blockingsWithBug = blockings.filter { blocking =>
            Minifier
              .checkMinifyDiff(strictCode, config.minifier)
              .getOrElse(false)
          }
          val blockingWithoutBug = blockings -- blockingsWithBug
          res ::= CovInvBugData(
            name,
            code,
            covered,
            blockingsWithBug,
            blockingWithoutBug,
            coveredNode,
            coveredBranch,
          )
        }
        res =
          res.sortBy(_.blockingsWithBug.size).sortBy(_.covered).sortBy(_.name)

        res.groupBy(_.name).foreach {
          case (name, covInvBugData) =>
            println(
              s"name: $name\n" +
              s"\ttotal: ${covInvBugData.size}\n" +
              s"\talive: ${covInvBugData.count(_.covered)}\n" +
              s"\tdead by bug: ${covInvBugData.count(_.blockingsWithBug.nonEmpty)}\n" +
              s"\tdead by no bug: ${covInvBugData.count(_.blockingsWithBug.isEmpty)}",
            )
        }

        for (filename <- config.out)
          dumpJson(
            data = res,
            filename = filename,
          )
        ()
      case None =>
        given ConInvDataEncoder: Encoder[CovInvData] = deriveEncoder

        val covDir = getFirstFilename(cmdConfig, "coverage-investigate")

        val cov = Coverage.fromLogSimpl(covDir, cfg)
        println(
          s"Coverage restored from $covDir (${cov.kFs}-F${if cov.cp then "CP"
          else ""}S)",
        )

        val targets = if config.useDB then
          val db = MinifierDB.fromResource
          for {
            (label, minimals) <- db.map
            minimal <- minimals
          } yield Script(minimal, label)
        else
          val newJsDir = getSecondFilename(cmdConfig, "coverage-investigate")
          for {
            jsFile <- listFiles(newJsDir)
            name = jsFile.getName
            if jsFilter(name)
            code = readFile(jsFile.getPath).drop(USE_STRICT.length).strip
          } yield
            val beginning = "const k = ( ( ) => { "
            val end = " } ) ( ) ;"
            val innerCode =
              code
                .replaceAll("\"use strict\"\\s*;?", "")
                .trim()
                .substring(beginning.length)
                .trim()
                .dropRight(end.length)
            println(innerCode)
            Script(innerCode, name)

        val res = (for {
          script <- targets.toList
        } yield {
          try
            cov.runAndCheckWithBlocking(script, modify = false) match
              case (
                    finalSt,
                    updated,
                    covered,
                    blockings,
                    coveredNode,
                    coveredBranch,
                  ) =>
                Some(
                  CovInvData(
                    script.name,
                    script.code,
                    covered,
                    blockings.map(_.code),
                    coveredNode.map(_.toString),
                    coveredBranch.map(_.toString),
                  ),
                )
          catch
            case e: Throwable =>
              println(s"Error in ${script.name}: ${e.getMessage}")
              // e.printStackTrace()
              None
        }).flatten.sortBy(_._2)

        res.foreach {
          case CovInvData(
                name,
                code,
                covered,
                blockings,
                coveredNode,
                coveredBranch,
              ) => {
            println(f"$name%30s: ${if (covered) "alive" else "dead"}")
          }
        }

        println(f"Total: ${res.size}%d, alive: ${res.count(_.covered)}%d")

        for (filename <- config.out)
          dumpJson(
            data = res,
            filename = filename,
          )
        ()

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
    (
      "use-db",
      BoolOption(c => c.useDB = true),
      "use resources/minifyfuzz-db minimals.",
    ),
    (
      "check-json",
      StrOption((c, s) => c.checkJson = Some(s)),
      "check the json file.",
    ),
    (
      "minifier",
      StrOption((c, s) => c.minifier = Some(s)),
      "minifier to use.",
    ),
  )

  class Config(
    var out: Option[String] = None,
    var useDB: Boolean = false,
    var checkJson: Option[String] = None,
    var minifier: Option[String] = None,
  )
}

sealed case class CovInvData(
  name: String,
  code: String,
  covered: Boolean,
  blockings: Set[String],
  coveredNode: Set[String],
  coveredBranch: Set[String],
)

sealed case class CovInvBugData(
  name: String,
  code: String,
  covered: Boolean,
  blockingsWithBug: Set[String],
  blockingWithoutBug: Set[String],
  coveredNode: Set[String],
  coveredBranch: Set[String],
)
