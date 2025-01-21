package esmeta.es.util.fuzzer

import scala.annotation.tailrec
import scala.collection.mutable.Map as MMap
import esmeta.util.BaseUtils.{computeChiSq, chiSqDistTable}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.util.SystemUtils.*
import esmeta.phase.MinifyFuzz

object FSTreeWrapper:
  def fromDir(baseDir: String, fixed: Boolean): FSTreeWrapper =
    given fsTreeConfigDecoder: Decoder[FSTreeConfig] = deriveDecoder
    val config = readJson[FSTreeConfig](f"$baseDir/fstrie-config.json")
    val fsTrieWrapper = FSTreeWrapper(config, fixed = fixed)
    fsTrieWrapper.replaceRootFromFile(f"$baseDir/fstrie-root.json")
    fsTrieWrapper

  def debug: Unit =
    val tree = FSTreeWrapper(
      FSTreeConfig(
        promotionThreshold = chiSqDistTable("0.01"),
        demotionThreshold = chiSqDistTable("0.05"),
        isSelective = true,
      ),
    )
    import tree.given
    val hitStacks = List(
      List("a"),
      List("a"),
      List("a"),
      List("a"),
    )
    val missStacks = List(
      List("b"),
      List("c"),
      List("d"),
      List("e"),
      List("f"),
      List("g"),
      List("h"),
      List("i"),
      List("j"),
      List("k"),
      List("l"),
      List("m"),
      List("n"),
      List("o"),
      List("p"),
      List("q"),
      List("r"),
      List("s"),
    )
    tree.touchWithHit(hitStacks)
    tree.touchWithMiss(missStacks)
    println(tree(List("a")))
    println(tree(List("b")))
    println(tree.root)

class FSTreeWrapper(
  val config: FSTreeConfig,
  val debug: Boolean = false,
  var fixed: Boolean = false,
) {
  val fixedSensMap = MMap.empty[List[String], Int]
  var familyMap = Map.empty[Int, FSTree] // familyMap: child -> parent
  def nextId = familyMap.size

  var root: FSTree =
    FSTree(status = FSTreeStatus.Noticed, depth = 0, isRoot = true, id = -1)
  var rootHits: Long = 0
  var rootMisses: Long = 0

  def sensDistr: Map[Int, Int] =
    root.stacks.groupBy(_.size).transform((_, v) => v.size).withDefault(_ => 0)

  def computeFeatureChiSq(
    hits: Long,
    misses: Long,
    pHits: Long,
    pMisses: Long,
  ): Double = {
    // root is noticed without any conditions
    val absentHits = pHits - hits
    val absentMisses = pMisses - misses
    val (chiSq, oddsRatio) =
      computeChiSq(hits, misses, absentHits, absentMisses)
    if (
      (hits + misses < config.minTouch) || (oddsRatio <= 1 && config.oneSided)
    ) then 0
    else
      assert(
        chiSq >= 0,
        f"Score for rootHits: $pHits, rootMisses: $pMisses, hits: $hits, misses: $misses is negative: $chiSq",
      )
      assert(
        chiSq.isFinite,
        f"Score for rootHits: $pHits, rootMisses: $pMisses, hits: $hits, misses: $misses is not finite: $chiSq",
      )
      chiSq
  }

  /** Insert feature stacks from a single script into the tree. The script
    * succeeded to invoke some non-trivial minifier operations. Increment the
    * hits of each node in the tree.
    *
    * @param stacks
    *   the feature stacks generated from the successful script
    */
  def touchWithHit(stacks: Iterable[List[String]]): Unit =
    if !fixed then
      stacks.foreach { s =>
        root.touchByStack(s.take(config.maxSensitivity), isHit = true)
      }
      rootHits += stacks.size

  /** Insert feature stacks from a single script into the tree. The script
    * failed to invoke some non-trivial minifier operations. Increment the
    * misses of each node in the tree.
    *
    * @param stacks
    *   the feature stacks generated from the failed script
    */
  def touchWithMiss(stacks: Iterable[List[String]]): Unit =
    if !fixed then
      stacks.foreach { s =>
        root.touchByStack(s.take(config.maxSensitivity), isHit = false)
      }
      rootMisses += stacks.size

  def apply(stack: List[String]): Int =
    val tmpStack = stack.take(config.maxSensitivity)
    if !config.isSelective then config.maxSensitivity
    else if fixed then fixedSensMap.getOrElseUpdate(tmpStack, root(tmpStack))
    else root(tmpStack)

  given fSTreeEncoder: Encoder[FSTree] = deriveEncoder
  given fsTreeDecoder: Decoder[FSTree] = deriveDecoder
  given fsTreeConfigEncoder: Encoder[FSTreeConfig] = deriveEncoder
  given fsTreeConfigDecoder: Decoder[FSTreeConfig] = deriveDecoder

  def replaceRootFromFile(filename: String): Unit =
    root = readJson[FSTree](filename)

  def stacks: Set[List[String]] = root.stacks

  def stacksWithScores: Map[List[String], Double] = root.stacksWithScores

  def stacksWithProbs = root.stacksWithProbs

  /** A tree that stores the status of each node in the tree, and calculates the
    * score of each node based on the hits and misses of the node and its
    * children. The score is used to determine the promotion or demotion of a
    * node. The fields are mutable for (maybe) better performance than using
    * immutable case classes and copying.
    *
    * @param children
    * @param status
    * @param hits
    * @param misses
    * @param promotables
    *   number of Promotable descendants of this node.
    * @param avgScore
    *   average score of its descendants which are Promotable. If this node is
    *   Promotable, the score is calculated based on the hits and misses of this
    *   node.
    * @param avgScoreSq
    */
  case class FSTree(
    private val id: Int,
    private val children: MMap[String, FSTree] = MMap.empty[String, FSTree],
    private var status: FSTreeStatus,
    private val depth: Int,
    var hits: Long = 0,
    var misses: Long = 0,
    private var chiSqValue: Double = 0,
    val isRoot: Boolean = false,
  ) {
    import FSTreeStatus.*

    /** How many features do we need to take from the stack?
      *
      * @param stack
      *   the stack of features' names
      * @return
      *   the number of features we need to take
      */
    private[FSTreeWrapper] def apply(stack: List[String]): Int =
      math.min(
        foldByStack(stack, -1) {
          case (acc, node) =>
            if node.status == Noticed then acc + 1
            else acc
        },
        config.maxSensitivity,
      )

    /** Insert a feature stack into the tree. Increment the hits or misses of
      * each node in the tree based on whether the node is hit or miss.
      */
    @tailrec
    private[FSTreeWrapper] final def touchByStack(
      stack: List[String],
      isHit: Boolean,
    ): Unit =
      if isHit then hits += 1 else misses += 1

      if (config.useLocalCorrelation)
        updateMyLocalChiSqValue()
      else
        updateMyGlobalChiSqValue()

      updateMyStatus()

      stack match {
        case Nil =>
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.touchByStack(tail, isHit)
            case None =>
              val childId = nextId
              val child = new FSTree(
                id = childId,
                children = MMap.empty[String, FSTree],
                status = Ignored,
                depth = depth + 1,
              )
              children(head) = child
              familyMap += (childId -> this)
              child.touchByStack(tail, isHit)
          }
      }

    private def updateMyLocalChiSqValue(): Unit =
      if (!isRoot)
        val parent = familyMap.getOrElse(
          this.id, {
            println("UNREACHABLE!!! No parent found for a non-root node")
            throw new RuntimeException("No parent found for a non-root node")
          },
        )
        val parentHits = parent.hits
        val parentMisses = parent.misses

        chiSqValue = computeFeatureChiSq(
          hits = hits,
          misses = misses,
          pHits = parentHits,
          pMisses = parentMisses,
        )

    private def updateMyGlobalChiSqValue(): Unit =
      if (!isRoot)
        chiSqValue = computeFeatureChiSq(
          hits = hits,
          misses = misses,
          pHits = rootHits,
          pMisses = rootMisses,
        )

    private def updateMyStatus(): Unit =
      if (!isRoot)
        status match {
          case Noticed =>
            if (chiSqValue < config.demotionThreshold) status = Ignored
          case _ =>
            if (chiSqValue > config.promotionThreshold) status = Noticed
        }

    /** Recursively do something to each node in the tree, starting from the
      * root
      */
    private def foreachFromRoot(
      op: FSTree => Unit,
      iterIgnored: Boolean = false,
    ): Unit =
      op(this)
      children.valuesIterator.foreach { child =>
        if iterIgnored || child.status != Ignored then
          child.foreachFromRoot(op, iterIgnored)
      }

    /** Recursively do something to each node in the tree, starting from the
      * leaf
      */
    private def foreachFromLeaf(
      op: FSTree => Unit,
      iterIgnored: Boolean = false,
    ): Unit =
      children.valuesIterator.foreach { child =>
        if iterIgnored || child.status != Ignored then
          child.foreachFromLeaf(op, iterIgnored)
      }
      op(this)

    @tailrec
    private final def iterByStack(stack: List[String])(
      op: FSTree => Unit,
    ): Unit =
      op(this)
      stack match {
        case Nil =>
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.iterByStack(tail)(op)
            case None        =>
          }
      }

    @tailrec
    private final def foldByStack[A](stack: List[String], z: A)(
      op: (A, FSTree) => A,
    ): A =
      stack match {
        case Nil => op(z, this)
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.foldByStack(tail, op(z, this))(op)
            case None        => z
          }
      }

    private def get(stack: List[String]): Option[FSTree] =
      foldByStack(stack, Option.empty[FSTree])((_, node) => Some(node))

    private def touches: Long = hits + misses

    def stacks = stacksSuppl(Nil)

    private def stacksSuppl(
      currStack: List[String],
    ): Set[List[String]] =
      if (status == Ignored) then Set.empty
      else
        children.flatMap {
          case (k, v) =>
            v.stacksSuppl(currStack :+ k)
        }.toSet + currStack

    def stacksWithScores: Map[List[String], Double] =
      stacksWithScoresSuppl(Nil)

    def stacksWithScoresSuppl(
      currStack: List[String],
    ): Map[List[String], Double] =
      if (status == Ignored) then Map.empty
      else
        children.flatMap {
          case (k, v) =>
            v.stacksWithScoresSuppl(currStack :+ k)
        }.toMap + (currStack -> chiSqValue)

    def stacksWithProbs = stacksWithProbsSuppl(Nil)

    def stacksWithProbsSuppl(
      currStack: List[String],
    ): Map[List[String], Double] =
      if children.isEmpty then
        Map(currStack -> (hits.toDouble / (hits + misses)))
      else if (status == Ignored) then Map.empty
      else
        children.flatMap {
          case (k, v) =>
            v.stacksWithProbsSuppl(currStack :+ k)
        }.toMap + (currStack -> (hits.toDouble / (hits + misses)))
  }
}

case class FSTreeConfig(
  promotionThreshold: Double = chiSqDistTable("0.01"),
  demotionThreshold: Double = chiSqDistTable("0.05"),
  maxSensitivity: Int = 3,
  minTouch: Int = 10,
  oneSided: Boolean = true,
  isSelective: Boolean = true,
  useSrv: Boolean = true,
  useLocalCorrelation: Boolean = false,
  doCleanup: Boolean = false,
)

/** Status of a node in the tree
  *   - Noticed: actively noticed; will not be demoted
  *   - Promotable: ignored but might be promoted to Noticed
  *   - Ignored: not noticed; will not be promoted
  */
enum FSTreeStatus:
  case Noticed
  case Ignored
