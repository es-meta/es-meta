package esmeta.ty

import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.annotation.tailrec

/** type modeling */
// TODO consider refactoring
case class TyModel(infos: Map[String, TyInfo] = Map()) {

  /** get method map */
  // TODO optimize
  def getMethod(tname: String): Map[String, String] = infos.get(tname) match {
    case Some(info) =>
      val parentMethods = info.parent.map(getMethod).getOrElse(Map())
      parentMethods ++ info.methods
    case None => Map()
  }

  /** direct subtypes */
  private lazy val directSubTys: Map[String, Set[String]] = {
    var children = Map[String, Set[String]]()
    for {
      (name, info) <- infos
      parent <- info.parent
      set = children.getOrElse(parent, Set())
    } children += parent -> (set + name)
    children
  }

  /** subtypes */
  lazy val subTys: Map[String, Set[String]] = {
    var descs = Map[String, Set[String]]()
    def aux(name: String): Set[String] = descs.get(name) match {
      case Some(set) => set
      case None =>
        val set = (for {
          sub <- directSubTys.getOrElse(name, Set())
          elem <- aux(sub)
        } yield elem) + name
        descs += name -> set
        set
    }
    infos.collect { case (name, TyInfo(None, _, _)) => aux(name) }
    descs
  }
  def isSubTy(l: String, r: String): Boolean =
    (l == r) || subTys.get(r).fold(false)(_ contains l)
  def isSubTy(l: String, rset: Set[String]): Boolean =
    rset.exists(r => isSubTy(l, r))
  def isSubTy(l: String, rset: BSet[String]): Boolean = rset match
    case Inf       => true
    case Fin(rset) => isSubTy(l, rset)
  def isSubTy(lset: Set[String], rset: Set[String]): Boolean =
    lset.forall(l => isSubTy(l, rset))

  /** loose subtyping relation between two value types */
  def isLooseSubTy(
    l: ValueTy,
    r: ValueTy,
  ): Boolean =
    val pureValue = isLooseSubTy(l.pureValue, r.pureValue)
    val normal = isLooseSubTy(l.normal, r.normal)
    val abrupt = l.abrupt <= r.abrupt
    pureValue && normal && abrupt

  /** loose subtyping relation between two pure value types */
  def isLooseSubTy(
    l: PureValueTy,
    r: PureValueTy,
  ): Boolean =
    val noName =
      ((l -- NameT.pureValue) <= (r -- NameT.pureValue))
    val name = ((l.name.set, r.name.set) match
      case (_, Inf) => true
      case (Inf, _) => false
      case (Fin(lset), Fin(rset)) =>
        lset.forall(l => rset.exists(r => isSubTy(l, r) || isSubTy(r, l)))
    )
    noName && name

  /** property map alias */
  type PropMap = Map[String, ValueTy]

  /** get types of property */
  def getProp(tname: String, p: String, check: Boolean = false): ValueTy =
    if (tname == "IntrinsicsRecord" && p.startsWith("%") && p.endsWith("%"))
      NameT("Object")
    else
      propMap
        .getOrElse(tname, Map())
        .getOrElse(
          p, {
            if (check) warn(s"unknown property access: $tname.$p")
            AbsentT
          },
        )

  /** property type */
  private lazy val propMap: Map[String, PropMap] = (for {
    name <- infos.keySet
  } yield name -> getPropMap(name)).toMap

  /** get property map */
  private def getPropMap(name: String): PropMap =
    val upper = getUpperPropMap(name)
    val lower = getLowerPropMap(name)
    lower.foldLeft(upper) {
      case (map, (k, t)) =>
        val newT = t || map.getOrElse(k, BotT)
        map + (k -> newT)
    }

  /** get property map from ancestors */
  private def getUpperPropMap(name: String): PropMap = infos.get(name) match
    case Some(info) =>
      val parentProps = info.parent.map(getUpperPropMap).getOrElse(Map())
      val props = info.props
      weakMerge(parentProps, props)
    case None => Map()

  /** get property map of name */
  private def getSamePropMap(name: String): PropMap =
    infos.get(name).map(_.props).getOrElse(Map())

  /** get subtypes with field existence */
  lazy val getSubTypes: ((String, String)) => List[String] =
    cached((name, key) =>
      val exist = getSamePropMap(name).contains(key)
      if (exist) List(name)
      else
        directSubTys
          .get(name)
          .fold(Nil)(_.flatMap(child => getSubTypes(child, key)).toList),
    )

  /** get property map from ancestors */
  private def getLowerPropMap(name: String): PropMap =
    directSubTys.get(name) match
      case Some(children) =>
        children
          .map(child => {
            val lower = getLowerPropMap(child)
            val props = getSamePropMap(child)
            weakMerge(lower, props)
          })
          .reduce(parallelWeakMerge)
      case None => getSamePropMap(name)

  /** weak merge */
  private def weakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, BotT)
        val rt = rmap.getOrElse(k, BotT)
        k -> (lt || rt)
      })
      .toMap
  }

  /** parallel weak merge */
  private def parallelWeakMerge(lmap: PropMap, rmap: PropMap): PropMap = {
    val keys = lmap.keySet ++ rmap.keySet
    keys.toList
      .map(k => {
        val lt = lmap.getOrElse(k, AbsentT)
        val rt = rmap.getOrElse(k, AbsentT)
        k -> (lt || rt)
      })
      .toMap
  }

}
object TyModel {

  /** alias */
  val EMPTY = ConstT("empty")
  val UNRESOLVABLE = ConstT("unresolvable")
  val LEXICAL = ConstT("lexical")
  val INITIALIZED = ConstT("initialized")
  val UNINITIALIZED = ConstT("uninitialized")
  val FIELD = ConstT("field")
  val METHOD = ConstT("method")
  val ACCESSOR = ConstT("accessor")
  val BASE = ConstT("base")
  val DERIVED = ConstT("derived")
  val STRICT = ConstT("strict")
  val GLOBAL = ConstT("global")
  val UNLINKED = ConstT("unlinked")
  val LINKING = ConstT("linking")
  val LINKED = ConstT("linked")
  val EVALUATING = ConstT("evaluating")
  val EVALUATING_ASYNC = ConstT("evaluating-async")
  val EVALUATED = ConstT("evaluated")
  val NUMBER = ConstT("Number")
  val BIGINT = ConstT("BigInt")
  val ALL = ConstT("all")
  val ALL_BUT_DEFAULT = ConstT("all-but-default")
  val NORMAL = ConstT("normal")
  val BREAK = ConstT("break")
  val CONTINUE = ConstT("continue")
  val RETURN = ConstT("return")
  val THROW = ConstT("throw")
  val SUSPENDED_START = ConstT("suspendedStart")
  val SUSPENDED_YIELD = ConstT("suspendedYield")
  val EXECUTING = ConstT("executing")
  val AWAITING_RETURN = ConstT("awaitingDASHreturn")
  val COMPLETED = ConstT("completed")
  val PENDING = ConstT("pending")
  val FULFILLED = ConstT("fulfilled")
  val REJECTED = ConstT("rejected")
  val FULFILL = ConstT("Fulfill")
  val REJECT = ConstT("Reject")
  val NAMESPACE_OBJ = ConstT("namespace-object")
  val NAMESPACE = ConstT("NAMESPACE")
}

/** type information */
case class TyInfo(
  parent: Option[String] = None,
  methods: Map[String, String] = Map(),
  fields: Map[String, String] = Map(),
) {
  lazy val props: Map[String, ValueTy] =
    val keys = methods.keySet ++ fields.keySet
    (for {
      k <- keys
      fs = fields.get(k).fold(BotT)(ValueTy.from)
      tys = methods.get(k) match
        case None         => fs
        case Some(method) => fs || CloT(method)
    } yield k -> tys).toMap
}
