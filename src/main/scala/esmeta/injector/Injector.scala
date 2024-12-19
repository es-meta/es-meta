package esmeta.injector

import esmeta.INJECT_LOG_DIR
import esmeta.cfg.CFG
import esmeta.interpreter.Interpreter
import esmeta.ir.*
import esmeta.es.*
import esmeta.es.builtin.INNER_MAP
import esmeta.spec.*
import esmeta.state.*
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.{LINE_SEP, RESOURCE_DIR}
import java.io.PrintWriter
import java.util.concurrent.TimeoutException
import scala.collection.mutable.{Map => MMap}
import scala.concurrent.TimeoutException
import scala.collection.mutable.ListBuffer

/** assertion injector */
object Injector {
  def apply(
    cfg: CFG,
    src: String,
    log: Boolean = false,
  ): ConformTest =
    val extractor = ExitStateExtractor(cfg.init.from(src))
    new Injector(cfg, extractor.result, log).result

  /** injection from files */
  def fromFile(
    cfg: CFG,
    filename: String,
    log: Boolean = false,
  ): ConformTest =
    val extractor = ExitStateExtractor(cfg.init.fromFile(filename))
    new Injector(cfg, extractor.result, log).result

  /** assertion definitions */
  lazy val header: String =
    val line = "// " + "-" * 77
    LINE_SEP +
    line + LINE_SEP +
    "// ASSERTION DEFINITIONS" + LINE_SEP +
    line + LINE_SEP +
    readFile(s"$RESOURCE_DIR/assertions.js").trim + LINE_SEP +
    line
}

/** extensible helper of assertion injector */
class Injector(
  cfg: CFG,
  exitSt: State,
  log: Boolean,
) {

  /** generated assertions */
  lazy val assertions: Vector[Assertion] =
    _assertions.clear
    if (normalExit)
      handleVariable // inject assertions from variables
      handleLet // inject assertions from lexical variables
    if (log)
      // pw.close
      println("[Injector] Logging finished")
    _assertions.toVector

  /** generated conformance test */
  lazy val conformTest: ConformTest = ConformTest(
    0,
    script.trim,
    exitTag,
    async,
    assertions,
  )

  /** injected script */
  lazy val result: ConformTest = conformTest

  /** target script */
  lazy val script = exitSt.sourceText.get

  /** exit status tag */
  lazy val exitTag: ExitTag = ExitTag(exitSt)

  /** normal termination */
  lazy val normalExit: Boolean = exitTag == ExitTag.Normal

  /** check whether it uses asynchronous features */
  // TODO more precise detection
  lazy val async: Boolean =
    script.contains("async") || script.contains("Promise")

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------
  // logging
  private lazy val pw: Unit = // PrintWriter =
    println(s"[Injector] Logging into $INJECT_LOG_DIR...")
    mkdir(INJECT_LOG_DIR)
    // getPrintWriter(s"$INJECT_LOG_DIR/log")
    ()
  private def log(data: Any): Unit = if (log) { 
    // pw.println(data); 
    // pw.flush() 
  }
  private def warning(msg: String): Unit = log(s"[Warning] $msg")

  // internal assertions
  private val _assertions: ListBuffer[Assertion] = ListBuffer()

  // handle variables
  private def handleVariable: Unit = for (x <- createdVars.toList.sorted) {
    log("handling variable...")
    val path = s"globalThis[\"$x\"]"
    getValue(s"""$globalMap["$x"].Value""") match
      case sv: SimpleValue => _assertions += HasValue(path, sv)
      case addr: Addr      => handleObject(addr, path)
      case _               => /* do nothing */
  }

  // get created variables
  private lazy val globalMap = s"@REALM.GlobalObject.$INNER_MAP"
  private lazy val globalThis =
    getValue(s"$globalMap.globalThis.Value")
  private lazy val createdVars: Set[String] =
    val initial = getStrKeys(getValue(s"@GLOBAL.$INNER_MAP"), "<global>")
    val current = getStrKeys(getValue(globalMap), "<global>")
    current -- initial

  // handle lexical variables
  private def handleLet: Unit = for (x <- createdLets.toList.sorted) {
    log("handling let...")
    getValue(s"""$lexRecord["$x"].BoundValue""") match
      case sv: SimpleValue => _assertions += HasValue(x, sv)
      case addr: Addr      => handleObject(addr, x)
      case _               => /* do nothing */
  }

  // handle addresses
  private def handleObject(addr: Addr, path: String): Unit =
    log(s"handleObject: $addr, $path")
    (addr, handledObjects.get(addr)) match
      case (_, Some(origPath)) =>
        _assertions += SameObject(addr, path, origPath)
      case (_: DynamicAddr, None) if addr != globalThis =>
        handledObjects += addr -> path
        exitSt(addr) match
          case (_: MapObj) =>
            handlePrototype(addr, path)
            handleExtensible(addr, path)
            handleCall(addr, path)
            handleConstruct(addr, path)
            handlePropKeys(addr, path)
            handleProperty(addr, path)
          case (_: RecordObj) =>
            handlePrototype(addr, path)
            handleExtensible(addr, path)
            handleCall(addr, path)
            handleConstruct(addr, path)
            handlePropKeys(addr, path)
            handleProperty(addr, path)
          case _ =>
      case _ =>
  private lazy val initHeap = cfg.init.initHeap
  private var handledObjects: Map[Addr, String] = (for {
    addr <- initHeap.map.keySet
    name <- addrToName(addr)
  } yield addr -> name).toMap
  private lazy val PREFIX_INTRINSIC = "INTRINSICS."
  private def addrToName(addr: Addr): Option[String] = addr match
    case a @ NamedAddr(name) if name.startsWith(PREFIX_INTRINSIC) =>
      Some(name.substring(PREFIX_INTRINSIC.length))
    case _ => None

  // handle [[Prototype]]
  private def handlePrototype(addr: Addr, path: String): Unit =
    log(s"handlePrototype: $addr, $path")
    access(addr, Str("Prototype")) match
      case addr: Addr => handleObject(addr, s"Object.getPrototypeOf($path)")
      case _          => warning("non-address [[Prototype]]: $path")

  // handle [[Extensible]]
  private def handleExtensible(addr: Addr, path: String): Unit =
    log(s"handleExtensible: $addr, $path")
    access(addr, Str("Extensible")) match
      case Bool(b) =>
        _assertions += IsExtensible(addr, path, b)
      case _ => warning("non-boolean [[Extensible]]: $path")

  // handle [[Call]]
  private def handleCall(addr: Addr, path: String): Unit =
    log(s"handleCall: $addr, $path")
    _assertions += IsCallable(addr, path, exists(addr, Str("Call")))

  // handle [[Construct]]
  private def handleConstruct(addr: Addr, path: String): Unit =
    log(s"handleConstruct: $addr, $path")
    _assertions += IsConstructable(
      addr,
      path,
      exists(addr, Str("Construct")),
    )

  // handle property names
  private def handlePropKeys(addr: Addr, path: String): Unit =
    log(s"handlePropKeys: $addr, $path")
    val newSt = exitSt.copied
    getValue(addr, "OwnPropertyKeys") match
      case Clo(f, _) =>
        newSt.context = Context(f, MMap(Name("O") -> addr))
        newSt.callStack = Nil
        try {
          // @TODO(@hyp3rflow): handle proxy correctly in interpreter
          Interpreter(newSt)
          val propsAddr = newSt(GLOBAL_RESULT) match
            case addr: Addr =>
              newSt(addr) match
                case obj @ RecordObj("CompletionRecord", _) =>
                  obj(Str("Value")) match
                    case addr: Addr => addr
                    case v          => error("not an address: $v")
                case _ => addr
            case v => error("not an address: $v")
          val len = newSt(propsAddr, Str("length")).asMath.toInt
          val array = (0 until len)
            .map(k => newSt(propsAddr, Math(k)))
            .flatMap(_ match {
              case Str(str)   => Some(s"'$str'")
              case addr: Addr => addrToName(addr)
              case _          => None
            })
          if (array.length == len)
            _assertions += CompareArray(addr, path, array)
        } catch {
          case e => warning("failed to interpret [[OwnPropertyKeys]]")
        }
      case _ => warning("non-closure [[OwnPropertyKeys]]: $path")

  // handle properties
  private lazy val fields =
    List("Get", "Set", "Value", "Writable", "Enumerable", "Configurable")
  private def handleProperty(addr: Addr, path: String): Unit =
    log(s"handleProperty: $addr, $path")
    val map = access(addr, Str(INNER_MAP))
    for (p <- getKeys(map, path)) access(map, p) match
      case addr: Addr =>
        exitSt(addr) match
          // NOTE : next line cannot be MapObj
          case RecordObj(
                "PropertyDescriptor" | "DataProperty" | "AccessorProperty",
                props,
              ) =>
            var desc: Map[String, SimpleValue] = Map.empty
            val2str(p).map(propStr => {
              for {
                field <- fields
                value <- props.get(field)
              } value match
                case sv: SimpleValue =>
                  desc += (field.toLowerCase -> sv)
                case addr: Addr =>
                  field match
                    case "Value" => handleObject(addr, s"$path[$propStr]")
                    case "Get" =>
                      handleObject(
                        addr,
                        s"Object.getOwnPropertyDescriptor($path, $propStr).get",
                      )
                    case "Set" =>
                      handleObject(
                        addr,
                        s"Object.getOwnPropertyDescriptor($path, $propStr).set",
                      )
                    case _ =>
                case _ => warning("invalid property: $path")
              _assertions += VerifyProperty(addr, path, propStr, desc)
            })
          case x => warning("invalid property: $path")
      case v => warning("invalid property: $path")

  // get values
  private def getValue(str: String): Value = getValue(Expr.from(str))
  private def getValue(expr: Expr): Value =
    (new Interpreter(exitSt.copied)).eval(expr)
  private def getValue(rt: RefTarget): Value = exitSt(rt)
  private def getValue(addr: Addr, prop: String): Value =
    getValue(FieldTarget(addr, Str(prop)))

  // access properties
  private def access(base: Value, props: Value*): Value =
    props.foldLeft(base) { case (base, p) => exitSt(base, p) }

  // check field existence
  private def exists(base: Value, field: Value): Boolean =
    exitSt.exists(base, field)

  // get created lexical variables
  private lazy val lexRecord = s"@REALM.GlobalEnv.DeclarativeRecord.$INNER_MAP"
  private lazy val createdLets: Set[String] =
    getStrKeys(getValue(lexRecord), "<global-decl-record>")

  // get keys
  private def getStrKeys(value: Value, path: String): Set[String] =
    getKeys(value, path).collect { case Str(p) => p }
  private def getKeys(value: Value, path: String): Set[Value] = value match
    case addr: Addr =>
      exitSt(addr) match
        case m: MapObj => m.map.keySet.toSet
        case _ => warning(s"[[$INNER_MAP]] is not a map object: $path"); Set()
    case _ => warning(s"[[$INNER_MAP]] is not an address: $path"); Set()

  // conversion to ECMAScript code
  private def val2str(value: Value): Option[String] = value match
    case sv: SimpleValue => Some(sv.toString)
    case addr: Addr      => addrToName(addr)
    case x               => None
}
