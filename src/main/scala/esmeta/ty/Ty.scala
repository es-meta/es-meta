package esmeta.ty

import esmeta.state.*
import esmeta.ty.util.Parser

/** types */
trait Ty extends TyElem {

  /** completion check */
  def isDefined: Boolean = this match
    case _: UnknownTy => false
    case _            => true

  /** completion check */
  def isCompletion: Boolean

  /** value containment check */
  def contains(value: Value, state: State): Boolean
}
object Ty extends Parser.From(Parser.ty)
