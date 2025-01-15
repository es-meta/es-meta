package esmeta.mutator.original

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.synthesizer.Synthesizer
import esmeta.synthesizer.original.*
import esmeta.es.util.{Walker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.state.Nearest
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.ty.AstSingleTy

class OriginalNearestMutator(using cfg: CFG)(
  val synthesizer: Synthesizer = OriginalRandomSynthesizer(cfg.grammar),
) extends OriginalMutator {

  val names = "NearestMutator" :: OriginalRandomMutator(synthesizer).names

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] = (for {
    (condView, cov) <- target
    CondView(cond, view) = condView
    nearest <- cov.targetCondViews.getOrElse(cond, Map()).getOrElse(view, None)
  } yield Walker(nearest, n).walk(ast).map((name, _)))
    .getOrElse(
      OriginalRandomMutator(synthesizer)(ast, n, target),
    )

  /** internal walker */
  class Walker(nearest: Nearest, n: Int)
    extends OriginalUtil.MultiplicativeListWalker {
    val AstSingleTy(name, rhsIdx, subIdx) = nearest.ty
    override def walk(ast: Syntactic): List[Syntactic] =
      if (
        ast.name == name &&
        ast.rhsIdx == rhsIdx &&
        ast.subIdx == subIdx &&
        ast.loc == Some(nearest.loc)
      )
        TotalWalker(ast, n)
      else
        super.walk(ast)
  }

  /** internal walker that mutates all internal nodes with same prob. */
  object TotalWalker extends OriginalUtil.AdditiveListWalker {
    var c = 0
    def apply(ast: Syntactic, n: Int): List[Syntactic] =
      val k = OriginalUtil.simpleAstCounter(ast)
      c = (n - 1) / k + 1
      shuffle(walk(ast)).take(n).toList

    override def walk(ast: Syntactic): List[Syntactic] =
      val mutants = super.walk(ast)
      List.tabulate(c)(_ => synthesizer(ast)) ++ mutants
  }
}
