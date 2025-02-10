package esmeta.es

import esmeta.ESMetaTest
import esmeta.cfgBuilder.CFGBuilder
import esmeta.compiler.Compiler
import esmeta.es.util.*
import esmeta.interpreter.*
import esmeta.ir.NormalInst
import esmeta.parser.AstFrom
import esmeta.spec.Spec
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import org.scalatest.Assertions.*

/** ECMAScript test */
trait ESTest extends ESMetaTest {
  def category: String = "es"
}
object ESTest {
  import ESMetaTest.*

  // file extension converter from .js to .ir
  lazy val js2ir = changeExt("js", "ir")

  // ---------------------------------------------------------------------------
  // parser helpers
  // ---------------------------------------------------------------------------
  // parse ES codes
  lazy val scriptParser: AstFrom = spec.scriptParser
  def parse(str: String): Ast = scriptParser.from(str)
  def parseFile(filename: String): Ast = scriptParser.fromFile(filename)

  // ---------------------------------------------------------------------------
  // interpreter helpers
  // ---------------------------------------------------------------------------
  // interpreter with additional assertion checks
  class CheckAfter(
    st: State,
    checkAfter: List[NormalInst],
  ) extends Interpreter(st):
    override lazy val result: State =
      while (step) {}
      for (assert <- checkAfter) super.eval(assert)
      st

  // eval ES codes
  def eval(
    sourceText: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = new CheckAfter(cfg.init.from(sourceText), checkAfter).result

  def evalFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = new CheckAfter(cfg.init.fromFile(filename), checkAfter).result

  // tests for ES parser
  def parseTest(ast: Ast): Ast =
    val newAst = parse(ast.toString(grammar = Some(grammar)))
    assert(ast == newAst)
    ast
  def parseTest(str: String): Ast = parseTest(parse(str))
  def parseFileTest(filename: String): Ast = parseTest(parseFile(filename))

  // tests for ES interpreter
  def checkExit(st: State): st.type = st(GLOBAL_RESULT) match
    case Undef => st
    case v     => fail(s"return not undefined: $v")
  def evalTest(
    str: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = checkExit(eval(str, checkAfter))

  def evalTestFile(
    filename: String,
    checkAfter: List[NormalInst] = Nil,
  ): State = checkExit(evalFile(filename, checkAfter))
}
