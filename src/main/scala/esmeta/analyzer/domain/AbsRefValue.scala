package esmeta.analyzer.domain

import esmeta.analyzer.*
import esmeta.ir.*

trait AbsRefValueDecl { self: Self =>

  /** basic abstract reference values */
  sealed trait AbsRefValue:
    override def toString: String = this match
      case AbsRefId(id)           => s"$id"
      case AbsRefProp(base, prop) => s"$base[$prop]"
  case class AbsRefId(id: Id) extends AbsRefValue
  case class AbsRefProp(base: AbsValue, prop: AbsValue) extends AbsRefValue
}
