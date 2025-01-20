package esmeta.es.builtin

import esmeta.es.*
import esmeta.cfg.CFG
import esmeta.state.*
import esmeta.ty.*

// XXX REMOVE after update to ES2025
/** model for symbols */
case class Symbol(cfg: CFG) {
  private def spec = cfg.program.spec
  given CFG = cfg

  private lazy val symbols: List[String] = (for {
    row <- spec.tables(WELL_KNOWN_SYMBOLS).rows
    symbolField <- row.headOption.map(_.stripPrefix("@@"))
  } yield symbolField)

  /** get symbol record */
  def ty: ValueTy = RecordT("", symbols.map(_ -> SymbolT).toMap)

  /** get symbol record */
  def obj: RecordObj = recordObj("")(
    (for { symField <- symbols } yield symField -> symbolAddr(symField)): _*,
  )

  /** get map for heap */
  def map: Map[Addr, Obj] = (for { symField <- symbols } yield symbolAddr(
    symField,
  ) -> recordObj("Symbol")("Description" -> Str(symbolName(symField)))).toMap
}
