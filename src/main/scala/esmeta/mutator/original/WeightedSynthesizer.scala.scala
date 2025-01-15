package esmeta.mutator.original

import esmeta.cfg.CFG
import esmeta.es.*
import esmeta.synthesizer.* // todo(@tmdghks): replace with original one
import esmeta.es.util.{Walker => AstWalker, *}
import esmeta.es.util.Coverage.*
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.ty.AstSingleTy

/** A nearest ECMAScript AST mutator */
class WeightedMutator(using cfg: CFG)(
  val pairs: (Mutator, Int)*,
) extends Mutator {

  /** mutate programs */
  def apply(
    ast: Ast,
    n: Int,
    target: Option[(CondView, Coverage)],
  ): Seq[(String, Ast)] = weightedChoose(pairs)(ast, n, target)

  val names = pairs.toList.flatMap(_._1.names).sorted.distinct
}
