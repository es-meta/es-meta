package esmeta.analyzer.tychecker

import esmeta.util.*
import esmeta.util.Appender.*
import esmeta.util.domain.*, Lattice.*

/** abstract return values */
trait AbsRetDecl { self: TyChecker =>

  case class AbsRet(value: AbsValue) extends Printable[AbsRet] {
    import AbsRet.*

    /** bottom element check */
    def isBottom: Boolean = value.isBottom

    /** partial order */
    def ⊑(that: AbsRet)(using AbsState): Boolean = this.value ⊑ that.value

    /** not partial order */
    def !⊑(that: AbsRet)(using AbsState): Boolean = !(this ⊑ that)

    /** join operator */
    def ⊔(that: AbsRet)(using AbsState): AbsRet =
      AbsRet(this.value ⊔ that.value)

    /** meet operator */
    def ⊓(that: AbsRet)(using AbsState): AbsRet =
      AbsRet(this.value ⊓ that.value)
  }
  object AbsRet extends RetDomain {

    /** top element */
    lazy val Top: AbsRet = AbsRet(AbsValue.Top)

    /** bottom element */
    lazy val Bot: AbsRet = AbsRet(AbsValue.Bot)

    /** appender */
    given rule: Rule[AbsRet] = (app, elem) => app >> elem.value

    extension (ret: AbsRet) {
      def value: AbsValue = ret.value
    }
  }
}
