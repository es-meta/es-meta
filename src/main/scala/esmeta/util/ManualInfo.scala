package esmeta.util

import esmeta.MANUALS_DIR
import esmeta.spec.Spec
import esmeta.test262.util.ManualConfig
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.io.File
import esmeta.ty.TyModel

/** manual information helpers */
case class ManualInfo(version: Option[Spec.Version]) {
  import ManualInfo.*

  /** get algorithm files */
  def algoFiles: List[File] = getAlgos(paths)

  /** get IR function files */
  def funcFiles: List[File] = getFuncs(paths)

  /** get compile rules */
  def compileRule: CompileRule = getCompileRule(paths)

  def tyModel: TyModel = getTyModel(version)

  /** get bugfixes */
  def bugfixFile: Option[File] = bugfixPath.map(File(_))
  def bugfixPath: Option[String] = getPath("bugfix.patch")

  /** get test262 */
  def test262: ManualConfig = ManualConfig(paths)

  /** get tycheck-ignore.json */
  def tycheckIgnore: Option[String] = getPath("tycheck-ignore.json")

  private def getAlgos(paths: List[String]): List[File] =
    getFiles(paths, algoFilter)
  private def getFuncs(paths: List[String]): List[File] =
    getFiles(paths, irFilter)
  private def getFiles(
    paths: List[String],
    filter: String => Boolean,
  ): List[File] = for {
    path <- paths
    file <- walkTree(s"$MANUALS_DIR/$path")
    if filter(file.getName)
  } yield file
  private def getPath(name: String): Option[String] =
    version.fold(None)(version => {
      val path = s"$MANUALS_DIR/${version.shortHash}/$name"
      if (exists(path)) Some(path) else None
    })
  private def getPathWithDefault(name: String): String =
    getPath(name).getOrElse(s"$MANUALS_DIR/default/$name")
  private def getCompileRule(paths: List[String]): CompileRule = paths
    .map(path => s"$MANUALS_DIR/$path/rule.json")
    .map(path => optional(readJson[CompileRule](path)).getOrElse(Map()))
    .foldLeft[CompileRule](Map())(_ ++ _)
  private def getTyModel(version: Option[Spec.Version]): TyModel =
    import esmeta.ty.util.JsonProtocol.given
    val path = getPathWithDefault("tymodel.json")
    readJson[TyModel](path)
  private lazy val paths: List[String] =
    List("default") ++ version.map(_.shortHash)
}
object ManualInfo:
  type CompileRule = Map[String, Map[String, String]]
