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
    val ast = cfg.scriptParser.from(code)
    walk(ast).toString(grammar = Some(grammar))

  val tracerSymbol: String = "t"
  def counterExp: String = s"${tracerSymbol}(${counter})"
  def counterStmt: String = s"${tracerSymbol}(${counter});"

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
      ast match
        case syn @ Syntactic("Block", _, _, _) => syn
        case _                                 => ???
    case Syntactic("FunctionBody", args, _, children) =>
      val walked =
        walkChildren(children).map(_.toString(grammar = Some(grammar)))
      val ast =
        esParser("FunctionBody", args).from(counterStmt + walked.mkString)
      ast match
        case syn @ Syntactic("FunctionBody", _, _, _) => syn
        case _                                        => ???
    case Syntactic("StatementList", args, _, children) =>
      val walked =
        walkChildren(children).map(_.toString(grammar = Some(grammar)))
      val ast =
        esParser("StatementList", args).from(walked.mkString + counterStmt)
      ast match
        case syn @ Syntactic("StatementList", _, _, _) => syn
        case _                                         => ???
    case _ => super.walk(syn)

}
