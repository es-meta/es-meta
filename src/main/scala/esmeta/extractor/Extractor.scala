package esmeta.extractor

import esmeta.*
import esmeta.lang.*
import esmeta.lang.{util => LangUtil}
import esmeta.spec.{*, given}
import esmeta.spec.util.{Parsers => SpecParsers}
import esmeta.ty.TyModel
import esmeta.util.ManualInfo
import esmeta.util.HtmlUtils.*
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.*
import scala.jdk.CollectionConverters._

/** specification extractor from ECMA-262 */
object Extractor:
  /** extracts a specification */
  def apply(
    document: Document,
    version: Option[Spec.Version],
    eval: Boolean,
  ): Spec = new Extractor(document, version, eval).result

  /** extracts a specification with a target version of ECMA-262 */
  def apply(targetOpt: Option[String] = None, eval: Boolean = false): Spec =
    val (version, document) = Spec.getVersionWith(targetOpt) {
      case version =>
        // if bugfix patch exists, apply it to spec.html
        lazy val document = readFile(SPEC_HTML).toHtml
        for {
          patchFile <- ManualInfo.bugfixPatchMap.get(version.hash)
        } {
          Spec.applyPatch(patchFile)
          document
          Spec.clean
        }
        document
    }
    apply(document, Some(version), eval)

  /** extracts a specification with a target version of ECMA-262 */
  def apply(target: String): Spec = apply(Some(target))

/** extensible helper of specification extractor from ECMA-262 */
class Extractor(
  document: Document,
  version: Option[Spec.Version] = None,
  eval: Boolean = false,
) extends SpecParsers {
  lazy val parser: LangUtil.Parsers =
    if (eval) LangUtil.ParserForEval else LangUtil.Parser

  /** final result */
  lazy val result =
    val spec = Spec(
      version = version,
      grammar = grammar,
      algorithms = algorithms,
      tables = tables,
      tyModel = tyModel, // TODO automatic extraction
    )
    // spec.document = document
    spec

  /** ECMAScript grammar */
  lazy val grammar = extractGrammar

  /** index map for ECMAScript grammar */
  lazy val idxMap = grammar.idxMap

  /** abstract algorithms in ECMA-262 */
  lazy val algorithms = extractAlgorithms

  /** tables in ECMA-262 */
  lazy val tables = extractTables

  /** type model */
  lazy val tyModel = extractTyModel

  /** extracts a grammar */
  def extractGrammar: Grammar = {
    val allProds = for {
      elem <- document.getElems("emu-grammar[type=definition]:not([example])")
      content = elem.html.trim.unescapeHtml
      prods = parse[List[Production]](content)
      prod <- prods
      inAnnex = elem.isInAnnex
    } yield (prod, inAnnex)
    val prods =
      (for ((prod, inAnnex) <- allProds if !inAnnex) yield prod).sorted
    val prodsForWeb =
      (for ((prod, inAnnex) <- allProds if inAnnex) yield prod).sorted
    Grammar(prods, prodsForWeb)
  }

  /** extracts algorithms */
  def extractAlgorithms: List[Algorithm] =
    // XXX load manually created algorithms
    var manualJobs = for {
      file <- ManualInfo.algoFiles
      content = readFile(file.toString)
      document = content.toHtml
      elem <- document.getElems("emu-alg:not([example])")
    } yield () => extractAlgorithm(elem)

    // extract algorithms in spec
    val jobs = for {
      elem <- document.getElems("emu-alg:not([example])")
    } yield () => extractAlgorithm(elem)

    concurrent(manualJobs ++ jobs).toList.flatten

  /** extracts an algorithm */
  def extractAlgorithm(elem: Element): List[Algorithm] = for {
    head <- extractHeads(elem)
    code = elem.html.unescapeHtml
    body = parser.parseBy(parser.step)(code)
    algo = Algorithm(head, body, code)
    // _ = algo.elem = elem
  } yield algo

  /** TODO ignores elements whose parents' ids are in this list */
  val IGNORE_ALGO_PARENT_IDS = Set(
    // TODO handle memory model
    "sec-weakref-execution",
    "sec-valid-chosen-reads",
    "sec-coherent-reads",
    "sec-tear-free-aligned-reads",
    "sec-races",
    "sec-data-races",
  )

  /** extracts algorithm heads */
  def extractHeads(elem: Element): List[Head] = {
    var parent = elem.parent
    // TODO more general rules
    if (
      (parent.id endsWith "statement-rules") ||
      (parent.id endsWith "expression-rules")
    ) parent = parent.parent

    // checks whether it is an algorithm that should be ignored
    if (IGNORE_ALGO_PARENT_IDS contains parent.id) return Nil

    // checks whether it is a valid algorithm head
    if (parent.tagName != "emu-clause") return Nil

    // consider algorithm head types using `type` attributes
    val headList = parent.attr("type") match {
      case "abstract operation" =>
        extractAbsOpHead(parent, elem, false)
      case "implementation-defined abstract operation" =>
        // TODO handle `implementation-defined abstract operation`
        // It is different with `host-defined abstract operation`
        extractAbsOpHead(parent, elem, true)
      case "host-defined abstract operation" =>
        extractAbsOpHead(parent, elem, true)
      case "numeric method" =>
        extractNumMethodHead(parent, elem)
      case "sdo" =>
        extractSdoHead(parent, elem)
      case "concrete method" =>
        extractConcMethodHead(parent, elem)
      case "internal method" =>
        extractInMethodHead(parent, elem)
      case _ =>
        extractUnusualHead(parent, elem)
    }

    headList.foreach(head =>
      val siblings = parent.children().asScala.filter(_.tagName == "emu-alg")
      head.emuClauseId = s"${parent.id}[${siblings.indexWhere(_ == elem)}]",
    )
    headList
  }

  /** extracts tables */
  def extractTables: Map[String, Table] = (for {
    elem <- document.getElems("emu-table")
    id = elem.getId
    datas = (for {
      row <- elem.getElems("tr")
    } yield row.getChildren.map(_.text)).toList
  } yield id -> Table(id, datas.head, datas.tail)).toMap

  /** extracts a type model */
  def extractTyModel: TyModel = ManualInfo.tyModel // TODO automatic extraction

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // get abstract operation heads
  private def extractAbsOpHead(
    parent: Element,
    elem: Element,
    isHostDefined: Boolean,
  ): List[AbstractOperationHead] =
    val headContent = getHeadContent(parent)
    val generator = parseBy(absOpHeadGen)(headContent)
    List(generator(isHostDefined))

  // get numeric method heads
  private def extractNumMethodHead(
    parent: Element,
    elem: Element,
  ): List[NumericMethodHead] =
    val headContent = getHeadContent(parent)
    List(parseBy(numMethodHead)(headContent))

  // get syntax-directed operation (SDO) heads
  private def extractSdoHead(
    parent: Element,
    elem: Element,
  ): List[SyntaxDirectedOperationHead] = {
    val headContent = getHeadContent(parent)
    val prevContent = elem.getPrevContent
    val defaultCaseStr =
      "Every grammar production alternative in this specification which is " +
      "not listed below implicitly has the following default definition of"
    val generator = parseBy(sdoHeadGen)(headContent)
    // to handle "default" case algorithms
    if (!prevContent.startsWith(defaultCaseStr)) {
      // normal case
      for {
        prod <- parse[List[Production]](prevContent)
        lhsName = prod.lhs.name
        rhs <- prod.rhsVec
        rhsName <- rhs.allNames
        syntax = lhsName + ":" + rhsName
        (idx, subIdx) = idxMap(syntax)
        rhsParams = rhs.params
        target = SyntaxDirectedOperationHead.Target(
          lhsName,
          idx,
          subIdx,
          rhsParams,
        )
      } yield generator(Some(target))
    } else {
      // special 'Default' case: assigned to special LHS named "Default")
      List(generator(None))
    }
  }

  // get concrete method heads
  private def extractConcMethodHead(
    parent: Element,
    elem: Element,
  ): List[ConcreteMethodHead] =
    val headContent = getHeadContent(parent)
    val generator = parseBy(concMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    List(generator(parseBy(paramDesc)(forData)))

  // get internal method heads
  private def extractInMethodHead(
    parent: Element,
    elem: Element,
  ): List[InternalMethodHead] =
    val headContent = getHeadContent(parent)
    val generator = parseBy(inMethodHeadGen)(headContent)
    val dataMap = elem.getPrevElem.toDataMap
    val forData = dataMap("for")
    List(generator(parseBy(paramDesc)(forData)))

  // get built-in heads
  private def extractBuiltinHead(
    parent: Element,
    elem: Element,
  ): List[BuiltinHead] =
    val headContent = getHeadContent(parent)
    val prevContent = elem.getPrevContent
    val heads = List(parseBy(builtinHead)(headContent))
    prevContent match
      case builtinHeadPattern(name) =>
        heads.map(_.copy(params = List(Param(name, UnknownType))))
      case _ => heads

  // handle unusual heads
  private lazy val builtinHeadPattern =
    ".*a built-in function that takes an argument _(\\w+)_.*".r
  private lazy val thisValuePattern =
    "The abstract operation (this\\w+Value) takes argument _(\\w+)_.*".r
  private lazy val aliasPattern =
    "means? the same thing as:".r
  private lazy val anonBuiltinPattern =
    "When a ([A-Za-z.` ]+) is called with argument _(\\w+)_,.*".r
  private def extractUnusualHead(
    parent: Element,
    elem: Element,
  ): List[Head] = elem.getPrevText match
    case thisValuePattern(name, param) =>
      List(
        AbstractOperationHead(
          false,
          name,
          List(Param(param, UnknownType)),
          UnknownType,
        ),
      )
    case aliasPattern() => extractAbsOpHead(parent, elem, false)
    case anonBuiltinPattern(name, param) =>
      val rname = name.trim.split(" ").map(_.capitalize).mkString
      val ref = BuiltinPath.YetPath(rname)
      List(BuiltinHead(ref, List(Param(param, UnknownType)), UnknownType))
    case _ if parent.hasAttr("aoid") => Nil
    case _                           => extractBuiltinHead(parent, elem)

  // get head contents from parent elements
  private def getHeadContent(parent: Element): String =
    parent.getFirstChildContent
}
