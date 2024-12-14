package esmeta.spec

import esmeta.compiler.Compiler
import esmeta.lang.*
import esmeta.parser.{ESParser, AstFrom}
import esmeta.spec.util.*
import esmeta.ty.*
import esmeta.util.BaseUtils.*
import esmeta.util.Git
import esmeta.util.HtmlUtils.*
import esmeta.{error => ESMetaError, *}
// import org.jsoup.nodes.Document

// import scala.scalajs.js.annotation.*

/** ECMAScript specifications (ECMA-262) */
case class Spec(
  version: Option[Spec.Version] = None, // git version
  grammar: Grammar = Grammar(), // lexical/syntactic grammar productions
  algorithms: List[Algorithm] = Nil, // abstract algorithms for semantics
  tables: Map[String, Table] = Map(), // tables
  tyModel: TyModel = TyModel(), // type models
) extends SpecElem {

  /** HTML Document element */
  // var document: Document = Document("")

  /** ECMAScript parser */
  lazy val esParser: ESParser = ESParser(grammar)
  lazy val scriptParser: AstFrom = esParser("Script")

  /** get incomplete algorithms */
  lazy val incompleteAlgorithms: List[Algorithm] =
    algorithms.filter(!_.complete)

  /** get complete algorithms */
  lazy val completeAlgorithms: List[Algorithm] =
    algorithms.filter(_.complete)

  /** get all algorithm steps */
  lazy val allSteps: List[Step] = for {
    algo <- algorithms
    step <- algo.steps
  } yield step

  /** get incomplete algorithm steps */
  lazy val incompleteSteps: List[Step] =
    allSteps.filter(_.isInstanceOf[YetStep])

  /** get complete algorithm steps */
  lazy val completeSteps: List[Step] =
    allSteps.filter(!_.isInstanceOf[YetStep])

  /** get all types */
  lazy val types: List[Type] = for {
    algo <- algorithms
    ty <- algo.retTy :: algo.head.funcParams.map(_.ty)
  } yield ty

  /** get known types */
  lazy val knownTypes: List[Type] =
    types.collect { case ty @ Type(_: ValueTy) => ty }

  /** get known types */
  lazy val yetTypes: List[Type] =
    types.collect { case ty @ Type(UnknownTy(Some(_))) => ty }

  /** mapping from algorithms names to algorithms */
  lazy val fnameMap: Map[String, Algorithm] =
    (for (algo <- algorithms) yield algo.head.fname -> algo).toMap

  /** get stats */
  lazy val stats: Stats = new Stats(this)

  /** get summary */
  lazy val summary: Summary = Summary(this)

  /** get an algorithm by id attribute */
  def getAlgoById(id: String): Algorithm =
    ???
  //   algorithms.filter(_.elem.getId == id) match
  //     case algo :: Nil => algo
  //     case _           => error(s"no algorithms found for $id")

  /** empty check */
  def isEmpty: Boolean =
    version == None &&
    grammar == Grammar() &&
    algorithms.isEmpty &&
    tables.isEmpty &&
    tyModel == TyModel()

  /** ECMAScript version string */
  lazy val versionString: String = version.fold("<none>")(_.toString)
}
object Spec extends Git(ECMA262_DIR)
