package esmeta.js.minifier

import java.util.StringJoiner
import scala.util.*
import scala.sys.process.*
import esmeta.LINE_SEP
import esmeta.error.*
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import java.util.concurrent.TimeoutException
import esmeta.es.util.USE_STRICT

import sttp.client4.*
import java.net.URLEncoder
import sttp.model.StatusCode

object Minifier {
  val minifyCmd = Map(
    "swc" -> "minify-runner -v swc@1.3.10",
    "swcES2015" -> "minify-runner -v swc@1.3.10 --notcompress -t es2015",
    "terser" -> "minify-runner -v terser@5.15.1",
    "babel" -> "minify-runner -v babel@7.19.1",
    "checkDiffSwc" -> "minify-runner -v swc@1.3.10 -d",
    "checkDiffSwcES2015" -> "minify-runner -v swc@1.3.10 --notcompress -t es2015 -d",
    "checkDiffTerser" -> "minify-runner -v terser@5.15.1 -d",
    "checkDiffBabel" -> "minify-runner -v babel@7.19.1 -d",
  )

  lazy val useSwc: Boolean = minifySwc(";").isSuccess

  private var hasWarned = false
  def warnUnspecified(): Unit =
    if !hasWarned then
      println("No minifier specified. Using SWC as default.")
      hasWarned = true

  def execScript(
    command: String,
    src: String,
    timeout: Option[Int] = None,
  ): Try[String] = Try {
    val escapedSrc = escapeToShellString(src)
    val stdout = new StringJoiner(LINE_SEP)
    val stderr = new StringJoiner(LINE_SEP)
    def cmd(main: String) = timeout match
      case Some(timeout) => s"timeout ${timeout}s $main $escapedSrc"
      case None          => s"$main $escapedSrc"
    val pb: ProcessBuilder = if command.contains("|") then {
      val Array(main, envInfo) = command.split("\\|")
      val Array(envKey, envVal) = envInfo.split(":")
      Process(cmd(main), None, envKey -> envVal)
    } else cmd(command)

    pb ! ProcessLogger(
      out => stdout.add(out),
      err => stderr.add(err),
    ) match {
      case 0         => stdout.toString
      case 124 | 137 => throw TimeoutException(command)
      case 127       => throw NoCommandError(command)
      case st        => throw new Exception(stdout.toString + stderr.toString)
    }
  }

  def minifySwc(src: String): Try[String] = execScript(minifyCmd("swc"), src)

  def minify(src: String, cmd: Option[String]): Try[String] =
    val minifierCode = cmd match
      case Some("swc") | Some("Swc")             => "swc"
      case Some("terser") | Some("Terser")       => "terser"
      case Some("babel") | Some("Babel")         => "babel"
      case Some("swcES2015") | Some("SwcES2015") => "swcES2015"
      case None =>
        warnUnspecified()
        "swc"
      case _ => throw new Exception("Invalid minifier specified.")
    execScript(minifyCmd(minifierCode), src)

  def checkMinifyDiffSwc(code: String): Option[Boolean] =
    checkMinifyDiff(code, Some("swc"))

  def checkMinifyDiff(code: String, cmd: Option[String]): Option[Boolean] =
    val minifierCode = cmd match
      case Some("swc") | Some("Swc")       => "checkDiffSwc"
      case Some("terser") | Some("Terser") => "checkDiffTerser"
      case Some("babel") | Some("Babel")   => "checkDiffBabel"
      case Some("swcES2015") | Some("SwcES2015") =>
        "checkDiffSwcES2015"
      case None =>
        warnUnspecified()
        "checkDiffSwc"
      case _ => throw new Exception("Invalid minifier specified.")
    try {
      val result = execScript(minifyCmd(minifierCode), code)
      result match {
        case Success(minifiedAndDiff) =>
          val diffResult = minifiedAndDiff.split(LINE_SEP).last
          if diffResult == "true" then Some(true)
          else if diffResult == "false" then Some(false)
          else {
            throw new Exception(s"Invalid diff result: $diffResult")
          }
        case Failure(exception) =>
          // println(s"[minify-check] $code $exception")
          None
      }
    } catch {
      case err => None
    }

  def checkMinifyDiffSrv(code: String, cmd: Option[String]): Option[Boolean] =
    try {
      val diffResult = MinifyServer.queryDiff(code, cmd).split(LINE_SEP).last
      if diffResult == "true" then Some(true)
      else if diffResult == "false" then Some(false)
      else {
        throw new Exception(s"Invalid diff result: $diffResult")
      }
    } catch {
      case err => None
    }
}

object MinifyServer {
  val serverUrl = "http://127.0.0.1:8282/"
  val backend = DefaultSyncBackend()

  def queryDiff(code: String, cmd: Option[String]): String = {
    val version = cmd match
      case Some("swc") | Some("Swc") | Some("swcES2015") | Some("SwcES2015") =>
        "swc@1.3.10"
      case Some("terser") | Some("Terser") => "terser@5.15.1"
      case Some("babel") | Some("Babel")   => "babel@7.19.1"
      case None =>
        Minifier.warnUnspecified()
        "swc@1.3.10"
      case _ => throw new Exception("Invalid minifier specified.")

    val encodedCode = URLEncoder.encode(code, "UTF-8")

    val additionalOptions = cmd match
      case Some("swcES2015") | Some("SwcES2015") =>
        "&notcompress=true&target=es2015"
      case _ => ""
    val url =
      s"$serverUrl?codeOrFilePath=$encodedCode&version=$version&diff=true" + additionalOptions
    val request = basicRequest.get(uri"$url")

    val response = request.send(backend)
    val endTime = System.currentTimeMillis()

    response.body match {
      case Right(output) => output
      case Left(error) =>
        if response.code == StatusCode(400) then
          println(s"MinifyServer Error: ${response.code} $error, with $code");
          "false"
        else "false"
    }
  }
}
