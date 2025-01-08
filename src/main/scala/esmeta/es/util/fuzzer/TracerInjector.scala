package esmeta.es.util.fuzzer

import esmeta.cfg.CFG
import esmeta.mutator.Mutator
import esmeta.es.*
import esmeta.es.Ast
import esmeta.es.util.Coverage.CondView
import esmeta.es.util.Coverage
import esmeta.es.util.Walker

class TracerInjector(using cfg: CFG) extends Walker {
  val targetSyns: List[String] = List("StatementList")
  val grammar = cfg.grammar
  val esParser = cfg.esParser

  var _counter: Int = 0
  def counter: Int = {
    _counter += 1
    _counter
  }

  def apply(code: String) =
    _counter = 0
    try {
      val ast = cfg.scriptParser.from(code)
      walk(ast).toString(grammar = Some(grammar))
    } catch {
      case e: Exception => throw new Exception(s"Error parsing code: $code", e)
    }

  def counterStmt: String = s"$TRACER_SYMBOL($counter);"

  def walkChildren(children: Vector[Option[Ast]]) =
    for {
      childOpt <- walkVector(children, walkOpt(_, walk))
      child <- childOpt
    } yield child

  override def walk(syn: Syntactic): Syntactic = syn match
    case Syntactic("Block", args, _, children) =>
      val walked =
        walkChildren(children).map(_.toString(grammar = Some(grammar)))
      val ast =
        esParser("Block", args).from("{" + counterStmt + walked.mkString + "}")
      ast.asInstanceOf[Syntactic]
    case Syntactic("FunctionBody", args, _, children) =>
      val walked =
        walkChildren(children).map(_.toString(grammar = Some(grammar)))
      val ast =
        esParser("FunctionBody", args).from(counterStmt + walked.mkString)
      ast.asInstanceOf[Syntactic]
    case Syntactic("StatementList", args, _, children) =>
      val walked =
        walkChildren(children).map(_.toString(grammar = Some(grammar)))
      val ast =
        esParser("StatementList", args).from(walked.mkString + counterStmt)
      ast.asInstanceOf[Syntactic]
    case _ => super.walk(syn)

}

class TracerExprInjector(using cfg: CFG) extends Walker {
  val targetSyns: List[String] = List("Expression")
  val grammar = cfg.grammar
  val esParser = cfg.esParser

  def apply(code: String) =
    try {
      val ast = cfg.scriptParser.from(code)
      walk(ast).toString(grammar = Some(grammar))
    } catch {
      case e: Exception => throw new Exception(s"Error parsing code: $code", e)
    }

  def walkChildren(children: Vector[Option[Ast]]) =
    for {
      childOpt <- walkVector(children, walkOpt(_, walk))
    } yield childOpt

  override def walk(syn: Syntactic): Syntactic =
    syn match
      case Syntactic("Expression", args, 0, children) =>
        val expr = Syntactic("Expression", args, 0, walkChildren(children))
          .toString(grammar = Some(grammar))
        val syn = esParser("Expression", args)
          .from(s"$TRACER_SYMBOL($expr)")
          .asInstanceOf[Syntactic]
        syn
      case Syntactic("Expression", args, 1, children) =>
        val expr = Syntactic("Expression", args, 1, walkChildren(children))
          .toString(grammar = Some(grammar))
        val syn = esParser("Expression", args)
          .from(s"$TRACER_SYMBOL(($expr))")
          .asInstanceOf[Syntactic]
        syn
      case _ => super.walk(syn)

}
