package esmeta.web

import esmeta.cfg.CFG
import esmeta.es.Initialize

def debugger: Debugger = _debugger.get
def initDebugger(cfg: CFG, sourceText: String): Unit =
  val cachedAst = cfg.scriptParser.from(sourceText)
  _debugger = Some(Debugger(Initialize(cfg, sourceText, Some(cachedAst))))
private var _debugger: Option[Debugger] = None

/** web server host */
val ESMETA_HOST = sys.env.getOrElse("ESMETA_HOST", "localhost")
