package esmeta.es.util.fuzzer

import scala.annotation.tailrec
import esmeta.util.BaseUtils.{computeChiSq, chiSqDistTable}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.util.SystemUtils.*

object TargetFeatureSet:
  val supportedTargetTranspilers = Set("swc", "babel", "terser", "swcES2015")
  val minTouch = 5

  var touchFeatureStacks: Map[String, Set[List[String]]] =
    Map.empty.withDefault(_ => Set.empty)

  given targetFeatureSetConfigDecoder: Decoder[SelectiveConfig] =
    deriveDecoder[SelectiveConfig]

  given targetFeatureSetConfigEncoder: Encoder[SelectiveConfig] =
    deriveEncoder[SelectiveConfig]

  given targetFeatureDataDecoder: Decoder[TargetFeatureData] =
    deriveDecoder[TargetFeatureData]

  given targetFeatureDataEncoder: Encoder[TargetFeatureData] =
    deriveEncoder[TargetFeatureData]

  given targetFeatureSetDecoder: Decoder[TargetFeatureSet] =
    deriveDecoder[TargetFeatureSet]

  given targetFeatureSetEncoder: Encoder[TargetFeatureSet] =
    deriveEncoder[TargetFeatureSet]

  def fromDir(baseDir: String): TargetFeatureSet =
    readJson[TargetFeatureSet](baseDir + "/tfset.json")

case class TargetFeatureSet(
  val config: SelectiveConfig,
  var rootHits: Long = 0,
  var rootMisses: Long = 0,
  var targetFeatureMap: Map[String, TargetFeatureData] = Map.empty,
) {

  // var featurePromotionMap: MMap[String, Int] =
  //   MMap.empty.withDefault(_ => 0)

  def apply(stack: List[String]): Int = {
    val tmpStack = stack.take(config.maxSensitivity)
    tmpStack.zipWithIndex.foldLeft(0) {
      case (acc, (feature, idx)) =>
        val featureData =
          targetFeatureMap.getOrElse(feature, TargetFeatureData())
        featureData.status match
          case TargetFeatureStatus.Ignored => acc
          case TargetFeatureStatus.Noticed => idx + 1
    }
  }

  // def flushPrmDemStacks(): (Set[List[String]], Set[List[String]]) = ???
  // val prmFeatures = featurePromotionMap.collect {
  //   case (k, v) if v > 0 => k
  // }.toSet
  // val demFeatures = featurePromotionMap.collect {
  //   case (k, v) if v < 0 => k
  // }.toSet
  // featurePromotionMap.clear()
  // (prmFeatures, demFeatures)

  def touchWithHit(stacks: Iterable[List[String]]): Unit =
    stacks.foreach {
      _.take(
        config.maxSensitivity,
      ).foreach { feature =>
        val tmpData = targetFeatureMap.getOrElse(feature, TargetFeatureData())
        tmpData.hit(
          rootHits,
          rootMisses,
          config,
        )
        targetFeatureMap += (feature -> tmpData)
      }
    }
    rootHits += stacks.size

  def touchWithMiss(stacks: Iterable[List[String]]): Unit =
    stacks.foreach {
      _.take(
        config.maxSensitivity,
      ).foreach { feature =>
        val tmpData = targetFeatureMap.getOrElse(feature, TargetFeatureData())
        tmpData.miss(
          rootHits,
          rootMisses,
          config,
        )
        targetFeatureMap += (feature -> tmpData)
      }
    }
    rootMisses += stacks.size

  def targetFeatureSize: Int =
    targetFeatureMap.filter(_._2.status == TargetFeatureStatus.Noticed).size

}

case class TargetFeatureData(
  private var hits: Long = 0,
  private var misses: Long = 0,
  var chiSqValue: Double = 0.0,
  var status: TargetFeatureStatus = TargetFeatureStatus.Ignored,
) {
  // returns true if the feature is promoted
  def hit(
    rootHits: Long,
    rootMisses: Long,
    config: SelectiveConfig,
  ): Option[UpdateResult] =
    hits += 1
    chiSqValue = computeFeatureChiSq(rootHits, rootMisses)
    updateStatus(config)

  // returns true if the feature is demoted
  def miss(
    rootHits: Long,
    rootMisses: Long,
    config: SelectiveConfig,
  ): Option[UpdateResult] =
    misses += 1
    chiSqValue = computeFeatureChiSq(rootHits, rootMisses)
    updateStatus(config)

  private def updateStatus(
    config: SelectiveConfig,
  ): Option[UpdateResult] =
    status match
      case TargetFeatureStatus.Noticed =>
        if chiSqValue < config.demotionThreshold then
          status = TargetFeatureStatus.Ignored
          Some(UpdateResult.Demoted)
        else None
      case TargetFeatureStatus.Ignored =>
        if chiSqValue > config.promotionThreshold then
          status = TargetFeatureStatus.Noticed
          Some(UpdateResult.Promoted)
        else None

  private def computeFeatureChiSq(
    rootHits: Long,
    rootMisses: Long,
  ): Double = {
    // root is noticed without any conditions
    val absentHits = rootHits - hits
    val absentMisses = rootMisses - misses
    val (chiSq, oddsRatio) =
      computeChiSq(hits, misses, absentHits, absentMisses)
    if ((hits + misses < TargetFeatureSet.minTouch) || (oddsRatio <= 1)) then 0
    else
      assert(
        chiSq >= 0,
        f"Score for rootHits: $rootHits, rootMisses: $rootMisses, hits: $hits, misses: $misses is negative: $chiSq",
      )
      assert(
        chiSq.isFinite,
        f"Score for rootHits: $rootHits, rootMisses: $rootMisses, hits: $hits, misses: $misses is not finite: $chiSq",
      )
      chiSq
  }
}

case class SelectiveConfig(
  promotionThreshold: Double = chiSqDistTable("0.01"),
  demotionThreshold: Double = chiSqDistTable("0.05"),
  maxSensitivity: Int = 2,
  targetTrans: String = "swc",
) {
  assert(
    promotionThreshold > demotionThreshold,
    "Promotion threshold should be greater than demotion threshold",
  )
  assert(
    maxSensitivity > 0,
    "Max sensitivity should be greater than 0",
  )
  assert(
    TargetFeatureSet.supportedTargetTranspilers.contains(targetTrans),
    f"Target transpiler $targetTrans is not supported",
  )
}

enum TargetFeatureStatus:
  case Noticed
  case Ignored

enum UpdateResult:
  case Promoted
  case Demoted
