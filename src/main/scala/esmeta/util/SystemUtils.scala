package esmeta.util

import java.io.{Reader, File, FileOutputStream, PrintWriter}
import java.nio.file.{Files, StandardCopyOption, Paths}
import java.util.concurrent.{Executors, ExecutorService}
import esmeta.*
import esmeta.error.*
import esmeta.util.BaseUtils.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.*
import scala.concurrent.duration.*
import scala.io.Source
import scala.sys.process.*
import scala.util.Try
import io.circe.*, io.circe.syntax.*, io.circe.parser.*
import cats.effect.{IO, Resource}
import scala.io.{BufferedSource, Source}
import cats.effect.unsafe.implicits.global

/** file utilities */
object SystemUtils {
  // encoding
  private val ENC = "utf8"

  /** file reader */
  def fileReader(filename: String): Reader =
    // Source.fromFile(filename, ENC).bufferedReader
    Resource
      .fromAutoCloseable(IO(Source.fromFile(filename).bufferedReader))
      .allocated // Acquires the resource
      .map(_._1) // Extracts the BufferedReader
      .unsafeRunSync() // Runs the IO and returns the BufferedReader directly

  /** file trees with filename */
  def walkTree(filename: String): Iterable[File] = walkTree(File(filename))

  /** file trees with files */
  def walkTree(file: File): Iterable[File] =
    Seq(file) ++ new Iterable[File] {
      def iterator: Iterator[File] =
        if (file.isDirectory) file.listFiles.iterator
        else Iterator.empty
    }.flatMap(walkTree)

  /** extension filter */
  def extFilter(ext: String): String => Boolean = _.endsWith(s".$ext")
  lazy val algoFilter = extFilter("algo")
  lazy val irFilter = extFilter("ir")
  lazy val cfgFilter = extFilter("cfg")
  lazy val jsFilter = extFilter("js")
  lazy val jsonFilter = extFilter("json")
  lazy val patchFilter = extFilter("patch")

  /** print writer */
  // def getPrintWriter(filename: String, append: Boolean = false): PrintWriter =
  //   val file = File(filename)
  //   val parent = file.getParent
  //   if (parent != null) mkdir(parent)
  //   val out = FileOutputStream(file, append)
  //   PrintWriter(out)

  /** dump given data to a file */
  def dumpFile(data: Any, filename: String): Unit =
    dumpFile(data, filename, false)

  /** dump given data to a file */
  def dumpFile(data: Any, filename: String, append: Boolean): Unit =
    //   val nf = getPrintWriter(filename, append)
    //   nf.print(data)
    //   nf.close()
    ()

  /** dump given data collection into a directory and show message */
  def dumpDir[T](
    name: String,
    iterable: Iterable[T],
    dirname: String,
    getName: T => String,
    getData: T => Any = (x: T) => x,
    remove: Boolean = false,
    append: Boolean = false,
    silent: Boolean = false,
  ): Unit =
    // mkdir(dirname, remove)
    // for (x <- iterable) dumpFile(getData(x), s"$dirname/${getName(x)}", append)
    // println(s"- Dumped $name into `$dirname` .")
    ()

  /** dump given data into a file and show message */
  def dumpFile(
    name: String,
    data: Any,
    filename: String,
    append: Boolean = false,
    silent: Boolean = false,
  ): Unit =
    // dumpFile(data, filename, append)
    if (!silent) println(s"- Dumped $name into `$filename` .")

  /** dump given data in a JSON format */
  def dumpJson[T](data: T, filename: String)(using Encoder[T]): Unit =
    dumpJson(data, filename, false)

  /** dump given data in a JSON format */
  def dumpJson[T](
    data: T,
    filename: String,
    noSpace: Boolean,
  )(using Encoder[T]): Unit =
    val json = data.asJson
    // dumpFile(if (noSpace) json.noSpaces else json.spaces2, filename, false)
    ()

  /** dump given data in a JSON format and show message */
  def dumpJson[T](
    name: String,
    data: T,
    filename: String,
    noSpace: Boolean = false,
    silent: Boolean = false,
  )(using Encoder[T]): Unit =
    dumpJson(data, filename, noSpace)
    if (!silent) println(s"- Dumped $name into `$filename` in a JSON format.")

  /** get first filename */
  def getFirstFilename(cmdConfig: CommandConfig, msg: String): String =
    cmdConfig.targets.headOption.getOrElse(throw NoFileError(msg))

  /** read file */
  def readFile(filename: String): String =
    def sourceResource: Resource[IO, BufferedSource] =
      Resource.fromAutoCloseable(IO(Source.fromFile(filename)))
    {
      sourceResource.use(source => IO(source.mkString))
    }.unsafeRunSync()

  /** read JSON */
  def readJsonContent[T](content: String)(implicit decoder: Decoder[T]): T =
    parse(content) match {
      case Left(err) => throw err
      case Right(json) =>
        json.as[T] match {
          case Left(err) => throw err
          case Right(v)  => v
        }
    }

  /** read JSON */
  def readJson[T](filename: String)(implicit decoder: Decoder[T]): T =
    parse(readFile(filename)) match {
      case Left(err) => throw err
      case Right(json) =>
        json.as[T] match {
          case Left(err) => throw err
          case Right(v)  => v
        }
    }

  /** read HTML */
  def readHtml(filename: String): org.jsoup.nodes.Document =
    import HtmlUtils.*
    readFile(filename).toHtml

  /** delete files */
  def deleteFile(filename: String): Unit = File(filename).delete

  /** change extension */
  def changeExt(from: String, to: String): String => String =
    filename => filename.substring(0, filename.length - from.length) + to

  /** get name without extension */
  def removedExt(filename: String): String =
    filename.split('.').dropRight(1).mkString(".")

  /** get extension */
  def getExt(filename: String): String =
    filename.split('.').last

  /** get absolute path */
  def getAbsPath(filename: String): String =
    Paths.get(filename).toAbsolutePath.normalize.toString

  /** renamed filename */
  def renameFile(from: String, to: String): Unit =
    File(from).renameTo(File(to))

  /** copy file */
  def copyFile(from: String, to: String): Unit = Files.copy(
    File(from).toPath,
    File(to).toPath,
    StandardCopyOption.REPLACE_EXISTING,
  )

  /** create symbolic link */
  def createSymLink(
    link: String,
    target: String,
    overwrite: Boolean = false,
  ): Unit =
    if (overwrite)
      deleteFile(link)
    Files.createSymbolicLink(
      File(link).toPath,
      File(target).toPath,
    )

  /** create directories */
  def mkdir(name: String, remove: Boolean = false): Unit =
    if (remove) rmdir(name)
    File(name).mkdirs

  /** remove directories */
  def rmdir(name: String): Unit = {
    def deleteRecursively(f: File): Boolean =
      if (f.isDirectory) f.listFiles match {
        case files: Array[File] => files.foreach(deleteRecursively)
        case null               =>
      }
      f.delete()
    deleteRecursively(File(name))
  }

  /** list directory */
  def listFiles(name: String): List[File] = listFiles(File(name))
  def listFiles(dir: File): List[File] =
    Option(dir.listFiles)
      .map(_.toList)
      .getOrElse(List())
      .filter(!_.getName.startsWith("."))

  /** file existence check */
  def exists(name: String): Boolean = File(name).exists

  /** check whether a shell command is normally terminated */
  def isNormalExit(str: String): Boolean = optional(executeCmd(str)).isDefined

  /** execute shell command with given dir, default to CUR_DIR */
  def executeCmd(cmd: String, dir: String = CUR_DIR): String =
    var directory = File(dir)
    var process = Process(Seq("sh", "-c", cmd), directory)
    val sb = new StringBuilder
    process !! ProcessLogger(s => (), s => ())

  /** set timeout with optional limitation */
  def timeout[T](f: => T, limit: Option[Int]): T =
    limit.fold(f)(l => timeout(f, l.second))

  /** set timeout with limitation */
  def timeout[T](f: => T, limit: Int): T =
    timeout(f, limit.seconds)

  /** set timeout with duration */
  def timeout[T](f: => T, duration: Duration): T =
    Await.result(Future(Try(f)), duration).get

  /** concurrently execute a list of functions */
  def concurrent[T](fs: Iterable[() => T], duration: Duration = Duration.Inf)(
    using ctxt: ExecutionContext = ExecutionContext.global,
  ): Iterable[T] = Await
    .result(Future.sequence(fs.map(f => Future(Try(f())))), duration)
    .map(_.get)

  /** use fixed thread pool */
  def fixedThread(nThread: Int): (ExecutorService, ExecutionContext) =
    val service = Executors.newFixedThreadPool(nThread)
    (service, ExecutionContext.fromExecutor(service))
}
