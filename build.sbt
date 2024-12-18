import sbtassembly.AssemblyPlugin.defaultUniversalScript

// ESMeta version
// NOTE: please update VERSION together in top-level package.scala
// NOTE: please update version info in the README.md file
ThisBuild / version := "0.5.1"

// Scala version
ThisBuild / scalaVersion := "3.3.3"

// ESMeta organization
ThisBuild / organization := "esmeta"

// Scala options
ThisBuild / scalacOptions := Seq(
  "-language:implicitConversions", // allow implicit conversions
  "-deprecation", // emit warning and location for usages of deprecated APIs
  "-feature", // emit warning for features that should be imported explicitly
  "-unchecked", // enable warnings where generated code depends on assumptions
)

// Java options
ThisBuild / javacOptions ++= Seq(
  "-encoding",
  "UTF-8",
)

// automatic reload build.sbt
Global / onChangedBuildSource := ReloadOnSourceChanges

// Metals requires the semanticdb compiler plugin
Global / semanticdbEnabled := true

// setting for sbt-ghpages with scaladoc
// NOTE: If you want to update gh-pages, use `ghpagesPushSite` command.
// Please carefully use it. See https://index.scala-lang.org/sbt/sbt-ghpages
enablePlugins(GhpagesPlugin)
enablePlugins(SiteScaladocPlugin)
git.remoteRepo := "git@github.com:es-meta/esmeta.git"

// general
lazy val complTest = taskKey[Unit]("Launch .completion validity tests (tiny)")

// basic
lazy val basicTest = taskKey[Unit]("Launch basic tests")

// size
lazy val tinyTest = taskKey[Unit]("Launch tiny tests (maybe milliseconds)")
lazy val smallTest = taskKey[Unit]("Launch small tests (maybe seconds)")
lazy val middleTest = taskKey[Unit]("Launch middle tests (maybe minutes)")
lazy val largeTest = taskKey[Unit]("Launch large tests (may hours)")

// extractor
lazy val extractorTest = taskKey[Unit]("Launch extractor tests")
lazy val extractorValidityTest =
  taskKey[Unit]("Launch validity tests for extractor (small)")

// spec
lazy val specTest = taskKey[Unit]("Launch spec tests")
lazy val specStringifyTest =
  taskKey[Unit]("Launch stringify tests for spec (tiny)")
lazy val specJsonTest = taskKey[Unit]("Launch JSON tests for spec (tiny)")

// lang
lazy val langTest = taskKey[Unit]("Launch lang tests")
lazy val langStringifyTest =
  taskKey[Unit]("Launch stringify tests for lang (tiny)")
lazy val langJsonTest = taskKey[Unit]("Launch JSON tests for lang (tiny)")

// ty
lazy val tyTest = taskKey[Unit]("Launch ty tests")
lazy val tyContainsTest = taskKey[Unit]("Launch contains tests for ty (tiny)")
lazy val tyStringifyTest =
  taskKey[Unit]("Launch stringify tests for ty (tiny)")
lazy val tyJsonTest = taskKey[Unit]("Launch JSON tests for ty (tiny)")
lazy val tyOpTest = taskKey[Unit]("Launch operation tests for ty (tiny)")

// compiler
lazy val compilerTest = taskKey[Unit]("Launch compiler tests")
lazy val compilerValidityTest =
  taskKey[Unit]("Launch validity tests for compiler (small)")

// ir
lazy val irTest = taskKey[Unit]("Launch ir tests")
lazy val irStringifyTest = taskKey[Unit]("Launch stringify tests for ir (tiny)")
lazy val irJsonTest = taskKey[Unit]("Launch JSON tests for ir (tiny)")

// cfgBuilder
lazy val cfgBuilderTest = taskKey[Unit]("Launch CFG builder tests")
lazy val cfgBuilderValidityTest =
  taskKey[Unit]("Launch validity tests for CFG builder (small)")

// cfg
lazy val cfgTest = taskKey[Unit]("Launch cfg tests")
lazy val cfgStringifyTest =
  taskKey[Unit]("Launch stringify tests for cfg (tiny)")

// interpreter
lazy val interpreterTest = taskKey[Unit]("Launch interpreter tests")
lazy val interpreterEvalTest =
  taskKey[Unit]("Launch eval tests for interpreter (tiny)")

// state
lazy val stateTest = taskKey[Unit]("Launch state tests")
lazy val stateStringifyTest =
  taskKey[Unit]("Launch stringify tests for state (tiny)")

// analyzer
lazy val analyzerTest = taskKey[Unit]("Launch analyzer tests")
lazy val analyzerTyCheckTest =
  taskKey[Unit]("Launch tycheck tests for analyzer (small)")

// es
lazy val esTest = taskKey[Unit]("Launch ECMAScript tests")
lazy val esEvalTest = taskKey[Unit]("Launch eval tests for ECMAScript (small)")
lazy val esParseTest =
  taskKey[Unit]("Launch parse tests for ECMAScript (small)")
lazy val esAnalyzeTest =
  taskKey[Unit]("Launch analyze tests for ECMAScript (small)")

// injector
lazy val injectorTest = taskKey[Unit]("Launch injector tests")
lazy val injectorStringifyTest =
  taskKey[Unit]("Launch stringify tests for injector (tiny)")

// test262
lazy val test262ParseTest =
  taskKey[Unit]("Launch parse tests for Test262 (large)")
lazy val test262EvalTest =
  taskKey[Unit]("Launch eval tests for Test262 (large)")

// Java options for assembly
lazy val assemblyJavaOpts = Seq(
  "-Xms1g",
  "-Xmx3g",
  "-XX:ReservedCodeCacheSize=512m",
  "-Xss4m",
  "-Dfile.encoding=utf8",
)

// assembly setting
ThisBuild / assemblyPrependShellScript := Some(
  assemblyJavaOpts.map("JAVA_OPTS=\"" + _ + " $JAVA_OPTS\"") ++
  defaultUniversalScript(shebang = false),
)

// Akka
val AkkaVersion = "2.6.19"
val AkkaHttpVersion = "10.2.8"

// project root
lazy val root = project
  .in(file("."))
  .settings(
    name := "esmeta",

    // libraries
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.14.1",
      "io.circe" %% "circe-generic" % "0.14.1",
      "io.circe" %% "circe-parser" % "0.14.1",
      "org.scalatest" %% "scalatest" % "3.2.11" % Test,
      "org.apache.commons" % "commons-text" % "1.9",
      "org.jsoup" % "jsoup" % "1.14.3",
      "org.jline" % "jline" % "3.13.3",
      "org.graalvm.polyglot" % "js" % "24.1.1" pomOnly (),
      ("org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2")
        .cross(CrossVersion.for3Use2_13),
      ("org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4")
        .cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion)
        .cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-stream" % AkkaVersion)
        .cross(CrossVersion.for3Use2_13),
      ("com.typesafe.akka" %% "akka-http" % AkkaHttpVersion)
        .cross(CrossVersion.for3Use2_13),
      ("ch.megard" %% "akka-http-cors" % "1.1.2")
        .cross(CrossVersion.for3Use2_13), // cors
      ("org.typelevel" %% "cats-core" % "2.12.0"),
    ),

    // Copy all managed dependencies to <build-root>/lib_managed/ This is
    // essentially a project-local cache.  There is only one lib_managed/ in
    // the build root (not per-project).
    retrieveManaged := true,

    // set the main class for 'sbt run'
    Compile / mainClass := Some("esmeta.ESMeta"),

    // test setting
    Test / testOptions += Tests
      .Argument("-fDG", baseDirectory.value + "/tests/detail"),
    Test / parallelExecution := true,

    // assembly setting
    assembly / test := {},
    assembly / assemblyOutputPath := file("bin/esmeta"),

    // fix deduplicate issue of polyglot dependencies
    // https://stackoverflow.com/questions/54834125/sbt-assembly-deduplicate-module-info-class
    assembly / assemblyMergeStrategy := {
      case PathList("module-info.class")               => MergeStrategy.last
      case path if path.endsWith("/module-info.class") => MergeStrategy.last
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },

    /** tasks for tests */
    // basic tests
    basicTest := (Test / testOnly)
      .toTask(
        List(
          "*TinyTest",
          "*SmallTest",
        ).mkString(" ", " ", ""),
      )
      .value,
    test := basicTest.dependsOn(format).value,
    // size
    tinyTest := (Test / testOnly).toTask(" *TinyTest").value,
    smallTest := (Test / testOnly).toTask(" *SmallTest").value,
    middleTest := (Test / testOnly).toTask(" *MiddleTest").value,
    largeTest := (Test / testOnly).toTask(" *LargeTest").value,
    // general
    complTest := (Test / testOnly).toTask(" *.Compl*Test").value,
    // extractor
    extractorTest := (Test / testOnly).toTask(" *.extractor.*Test").value,
    extractorValidityTest := (Test / testOnly)
      .toTask(" *.extractor.Validity*Test")
      .value,
    // spec
    specTest := (Test / testOnly).toTask(" *.spec.*Test").value,
    specStringifyTest := (Test / testOnly)
      .toTask(" *.spec.Stringify*Test")
      .value,
    specJsonTest := (Test / testOnly).toTask(" *.spec.Json*Test").value,
    // lang
    langTest := (Test / testOnly).toTask(" *.lang.*Test").value,
    langStringifyTest := (Test / testOnly)
      .toTask(" *.lang.Stringify*Test")
      .value,
    langJsonTest := (Test / testOnly).toTask(" *.lang.Json*Test").value,
    // ty
    tyTest := (Test / testOnly).toTask(" *.ty.*Test").value,
    tyContainsTest := (Test / testOnly).toTask(" *.ty.Contains*Test").value,
    tyStringifyTest := (Test / testOnly).toTask(" *.ty.Stringify*Test").value,
    tyJsonTest := (Test / testOnly).toTask(" *.ty.Json*Test").value,
    tyOpTest := (Test / testOnly).toTask(" *.ty.Op*Test").value,
    // compiler
    compilerTest := (Test / testOnly).toTask(" *.compiler.*Test").value,
    compilerValidityTest := (Test / testOnly)
      .toTask(" *.compiler.Validity*Test")
      .value,
    // ir
    irTest := (Test / testOnly).toTask(" *.ir.*Test").value,
    irStringifyTest := (Test / testOnly).toTask(" *.ir.Stringify*Test").value,
    irJsonTest := (Test / testOnly).toTask(" *.ir.Json*Test").value,
    // cfgBuilder
    cfgBuilderTest := (Test / testOnly).toTask(" *.cfgBuilder.*Test").value,
    cfgBuilderValidityTest := (Test / testOnly)
      .toTask(" *.cfgBuilder.Validity*Test")
      .value,
    // cfg
    cfgTest := (Test / testOnly).toTask(" *.cfg.*Test").value,
    cfgStringifyTest := (Test / testOnly).toTask(" *.cfg.Stringify*Test").value,
    // interpreter
    interpreterTest := (Test / testOnly).toTask(" *.interpreter.*Test").value,
    interpreterEvalTest := (Test / testOnly)
      .toTask(" *.interpreter.Eval*Test")
      .value,
    // state
    stateTest := (Test / testOnly).toTask(" *.state.*Test").value,
    stateStringifyTest := (Test / testOnly)
      .toTask(" *.state.Stringify*Test")
      .value,
    // analyzer
    analyzerTest := (Test / testOnly).toTask(" *.analyzer.*Test").value,
    analyzerTyCheckTest := (Test / testOnly)
      .toTask(" *.analyzer.TyCheck*Test")
      .value,
    // es
    esTest := (Test / testOnly).toTask(" *.es.*Test").value,
    esEvalTest := (Test / testOnly).toTask(" *.es.Eval*Test").value,
    esParseTest := (Test / testOnly).toTask(" *.es.Parse*Test").value,
    esAnalyzeTest := (Test / testOnly).toTask(" *.es.Analyze*Test").value,
    // ir
    injectorTest := (Test / testOnly).toTask(" *.injector.*Test").value,
    injectorStringifyTest := (Test / testOnly)
      .toTask(" *.injector.Stringify*Test")
      .value,
    // test262
    test262ParseTest := (Test / testOnly).toTask(" *.test262.Parse*Test").value,
    test262EvalTest := (Test / testOnly).toTask(" *.test262.Eval*Test").value,
  )

// create the `.completion` file for autocompletion in shell
lazy val genCompl = taskKey[Unit]("generate autocompletion file (.completion)")
genCompl := (Compile / runMain).toTask(" esmeta.util.GenCompl").value

// build for release with genCompl and assembly
lazy val release = taskKey[Unit]("release with format, genCompl, and assembly")
release := Def
  .sequential(
    format,
    assembly / assembly,
    genCompl,
  )
  .value

// format all files
lazy val format = taskKey[Unit]("format all files")
format := Def
  .sequential(
    Compile / scalafmtAll,
    Compile / scalafmtSbt,
  )
  .value

// format check all files
lazy val formatCheck = taskKey[Unit]("format check all files")
formatCheck := Def
  .sequential(
    Compile / scalafmtCheckAll,
    Compile / scalafmtSbtCheck,
  )
  .value
