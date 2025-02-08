package esmeta.es.util.fuzzer

import scala.util.*
import esmeta.es.*
import esmeta.parser.ESParser
import esmeta.spec.Spec
import akka.http.scaladsl.model.headers.CacheDirectives.`max-age`
import esmeta.js.minifier.Minifier
import esmeta.js.minifier.Minifier.checkMinifyDiff

object MinifyChecker {
  val swcMinifyFunction: String => Option[String] = code =>
    Minifier.minifySwc(code) match
      case Failure(exception) =>
        // println(s"[minify-check] $code $exception")
        None
      case Success(minified) => Some(minified)
}

class MinifyChecker(
  spec: Spec,
  minify: String => Option[String], // minify function
  config: MinifyCheckerConfig = MinifyCheckerConfig(),
  cmdOption: Option[String],
) {
  /*
   * Check if the given code is minified.
   */
  def check(code: String): Option[MinifyCheckResult] = minify(code).map {
    minified =>
      val diff = checkMinifyDiff(code, cmdOption).getOrElse(false)

      MinifyCheckResult(
        diff = diff,
        original = code,
        minified = minified,
      )
  }

  /*
   * Check the structural difference between the given ASTs and return every
   * different part of the ASTs, if any. The returned list should have the
   * changed parts of the original AST.
   *
   * Directly checking whether the string value of the code is the same as the
   * minified code is not enough because the minified code can have different
   * source code string while preserving the same Ast structure. So, we need to
   * check the structural equivalence ASTs of the code and the minified code.
   *
   * It would be appropriate to ignore the differences in the name of the
   * variables (i.e., the value of literals). (or it would be perfect if we
   * first rename all the variables of ast1 and ast2 and then compare them)
   */
  private def checkAstDiff(ast1: Ast, ast2: Ast): List[Ast] =
    checkAstDiffSuppl(ast1, ast2, Nil)

  /* Recursive helper function for checkAstDiff using Myers algorithm */
  def checkAstDiffSuppl(ast1: Ast, ast2: Ast, acc: List[Ast]): List[Ast] =
    val flattenedAst1 = ast1.flattenSyntactic
    val flattenedAst2 = ast2.flattenSyntactic

    myersDiff(flattenedAst1, flattenedAst2)

  private def myersDiffAux(
    flattenedAst1: List[Ast],
    flattenedAst2: List[Ast],
    acc: List[Ast],
  ): List[Ast] = Nil // Deprecated
  // val max = flattenedAst1.length + flattenedAst2.length
  // val v = Array.fill(2 * max + 1)(0)
  // val offset = max

  // var result = acc

  // // println("------flattenedAst1-----")
  // // flattenedAst1.foreach(ast => println(ast.name))
  // // println("------------------------")

  // // println("------flattenedAst2-----")
  // // flattenedAst2.foreach(ast => println(ast.name))
  // // println("------------------------")

  // // implement diff algorithm here
  // // partially implement just comparing ast names
  // // todo: make metrics for comparing asts
  // val n = flattenedAst1.length
  // val m = flattenedAst2.length

  // var d = 0
  // while (d <= max) do
  //   for (k <- -d to d by 2) do
  //     var i =
  //       if (k == -d || (k != d && v(offset + k - 1) < v(offset + k + 1)))
  //         v(offset + k + 1)
  //       else
  //         v(offset + k - 1) + 1
  //     var j = i - k
  //     while (
  //         i < n
  //         && j < m
  //         && flattenedAst1(i).name == flattenedAst2(j).name
  //       )
  //     do
  //       i += 1
  //       j += 1
  //     v(offset + k) = i
  //     if (i >= n && j >= m) then return result
  //     if (
  //         i < n && j < m
  //         && flattenedAst1(i).name != flattenedAst2(j).name
  //       )
  //     then result = flattenedAst1(j) :: result
  //   d += 1

  // println(s"different ast parts: ${result.map(_.name)}")
  // result

  private def myersDiff(
    flattenedAst1: List[Ast],
    flattenedAst2: List[Ast],
  ): List[Ast] = myersDiffAux(flattenedAst1, flattenedAst2, Nil)
}

case class MinifyCheckerConfig(
  // TODO: add configurations if needed
)

case class MinifyCheckResult(
  // influentialOptions: List[String], // TODO: check options that affect minification (not right now)
  diff: Boolean,
  original: String,
  minified: String,
)
