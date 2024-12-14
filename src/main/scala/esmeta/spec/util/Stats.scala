package esmeta.spec.util

import esmeta.LINE_SEP
import esmeta.spec.*
import esmeta.util.HtmlUtils.*
import esmeta.util.BaseUtils.*
import esmeta.lang.util.KindCollector
import esmeta.util.SystemUtils.*
import org.jsoup.nodes.*
import scala.collection.mutable.{Map => MMap}

/** specification statistics */
class Stats(spec: Spec) {

  /** dump statistics */
  def dumpTo(baseDir: String): Unit =
    mkdir(baseDir)
    println(s"- Dumped statistics info. into $baseDir")
    dumpFile(ElemStat.getAlgoString, s"$baseDir/algo-summary")
    dumpFile(ElemStat.getStepString, s"$baseDir/step-summary")
    dumpFile(KindStat.getStepString, s"$baseDir/step-kind-summary")
    dumpFile(KindStat.getExprString, s"$baseDir/expr-kind-summary")
    dumpFile(KindStat.getCondString, s"$baseDir/cond-kind-summary")

  /** statistics for pass/total */
  private case class PassStat(pass: Int = 0, total: Int = 0):
    def +(other: PassStat): PassStat = PassStat(
      pass + other.pass,
      total + other.total,
    )
    def fail: Int = total - pass
    override def toString =
      if total != 0 then s"${ratioString(pass, total)}" else ""
  private object PassStat:
    def apply(b: Boolean): PassStat = PassStat(if (b) 1 else 0, 1)

  /** statistics for elements in spec */
  object ElemStat {

    /** statistics for each elements */
    private case class ElemStat(
      algo: PassStat = PassStat(),
      step: PassStat = PassStat(),
    ) {
      def +(other: ElemStat): ElemStat = ElemStat(
        algo + other.algo,
        step + other.step,
      )
    }

    /** maps from element to stat */
    private val map: MMap[Element, ElemStat] = MMap()

    /** add algorithm */
    def +=(algo: Algorithm): Unit = {
      val completed = algo.completeSteps.length
      val total = algo.steps.length
      val stat = ElemStat(
        PassStat(completed == total),
        PassStat(completed, total),
      )
      // algo.elem.walkAncestor(
      //   elem =>
      //     val elemStat = map.getOrElse(elem, ElemStat())
      //     map += (elem -> (elemStat + stat))
      //   ,
      //   (),
      //   (a, b) => (),
      // )
    }

    /** get summary of element */
    private def getElementString(
      elem: Element,
      pstat: ElemStat => PassStat,
      yet: Boolean = false,
      indent: Int = 0,
    ): String = {

      // get new line string
      def newline(indent: Int = 0): String = LINE_SEP + "  " * indent

      // get pass stat
      val stat = pstat(map.getOrElse(elem, ElemStat()))

      // get yet steps
      val printYet = yet && stat.fail != 0 && !elem.children.toList
        .filter(
          _.tagName == "emu-alg",
        )
        .isEmpty
      val yetStepStr =
        if printYet then
          // get algos in same emu-clause
          // val algos = spec.algorithms.filter(_.elem.getId == elem.id)

          // get yet steps
          /* algos
            .map(_.incompleteSteps)
            .flatten
            .map(newline(indent + 2) + _.toString(false))
            .fold("")(_ + _) */
            ""
        else ""

      // final result
      if elem.tagName == "body" then stat.toString
      else if elem.tagName == "emu-clause" then
        s"${newline(indent)}- ${elem.id}:${stat}${yetStepStr}"
      else ""
    }

    /** getString */
    def getAlgoString: String =
      // XXX temp suppress
      ""
      // getString(spec.document.body, elem => elem.algo)
    def getStepString: String =
      // XXX temp suppress
      ""
      // getString(spec.document.body, elem => elem.step, true)
    private def getString(
      elem: Element,
      pstat: ElemStat => PassStat,
      yet: Boolean = false,
      indent: Int = 0,
    ): String =
      val elemStr = getElementString(elem, pstat, yet, indent)
      val children = elem.children.toList
      if !children.isEmpty then
        children
          .map(getString(_, pstat, yet, indent + 1))
          .fold(elemStr)(_ + _)
      else elemStr

  }

  /** statistics for kinds in spec */
  object KindStat {
    import KindCollector.*

    private val map: MMap[ClassName, KindData] = MMap()
    def sortedList: List[(ClassName, KindData)] = map.toList.sortBy(_._2.count)

    /** add algorithms */
    def +=(algo: Algorithm): Unit = for {
      (name, data) <- KindCollector(algo.body).map
      totalData = map.getOrElseUpdate(name, KindData())
    } totalData += data

    /** getString */
    def getStepString: String = getString(name => name.kind == Kind.Step)
    def getExprString: String = getString(name => name.kind == Kind.Expr)
    def getCondString: String = getString(name => name.kind == Kind.Cond)
    private def getString(
      filter: ClassName => Boolean,
    ): String = (for {
      (cname, data) <- sortedList if filter(cname)
    } yield f"${cname.name}%-40s${data.count}").mkString(LINE_SEP)
  }

  /** initialize */
  private def init: Unit = for { algo <- spec.algorithms } {
    KindStat += algo; ElemStat += algo
  }
  init

}
