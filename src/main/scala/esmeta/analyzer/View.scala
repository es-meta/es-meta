package esmeta.analyzer

import esmeta.cfg.*

trait ViewDecl { self: Analyzer =>

  /** view abstraction for analysis sensitivities */
  case class View(
    calls: List[Call] = Nil,
    loops: List[LoopCtxt] = Nil,
    intraLoopDepth: Int = 0,
    // TODO tys: List[Type] = Nil,
  ) extends AnalyzerElem {

    /** empty check */
    def isEmpty: Boolean = this == View()
  }

  /** loop context */
  case class LoopCtxt(loop: Branch, depth: Int)
}
