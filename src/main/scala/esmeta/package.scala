package esmeta

import esmeta.error.NoEnvVarError

/** line separator */
val LINE_SEP = System.getProperty("line.separator")

/** base project directory root */
val VERSION = "0.5.1"

enum Platform:
  case JS, JVM

/** base project directory root */
// XXX temp fix
val BASE_DIR =
  ""
// sys.env.getOrElse("ESMETA_HOME", throw NoEnvVarError)

/** log directory */
val LOG_DIR = s"$BASE_DIR/logs"
val EXTRACT_LOG_DIR = s"$LOG_DIR/extract"
val COMPILE_LOG_DIR = s"$LOG_DIR/compile"
val CFG_LOG_DIR = s"$LOG_DIR/cfg"
val ANALYZE_LOG_DIR = s"$LOG_DIR/analyze"
val INJECT_LOG_DIR = s"$LOG_DIR/inject"
val EVAL_LOG_DIR = s"$LOG_DIR/eval"
val FUZZ_LOG_DIR = s"$LOG_DIR/fuzz"
val TEST262TEST_LOG_DIR = s"$LOG_DIR/test262"
val DUMP_LOG_DIR = s"$LOG_DIR/dump"

/** stack trace depth */
val STACK_TRACE_DEPTH = 15

/** tests directory root */
val TEST_DIR = s"$BASE_DIR/tests"

/** specification directory */
val ECMA262_DIR = s"$BASE_DIR/ecma262"
val SPEC_HTML = s"$ECMA262_DIR/spec.html"

/** current directory root */
val CUR_DIR = System.getProperty("user.dir")

/** source code directory */
val SRC_DIR = s"$BASE_DIR/src/main/scala/esmeta"

/** resource directory */
val RESOURCE_DIR = s"$BASE_DIR/src/main/resources"
val UNICODE_DIR = s"$RESOURCE_DIR/unicode"
val MANUALS_DIR = s"$RESOURCE_DIR/manuals"
val RESULT_DIR = s"$RESOURCE_DIR/result"

/** package name */
val PACKAGE_NAME = "esmeta"

/** tests directory */
val IR_TEST_DIR = s"$TEST_DIR/ir"
val ES_TEST_DIR = s"$TEST_DIR/es"

/** error stack trace display mode */
var ERROR_MODE = false

/** exit status return mode */
var STATUS_MODE = false

/** test262 directories */
var TEST262_DIR = s"$TEST_DIR/test262"
def TEST262_TEST_DIR = s"$TEST262_DIR/test"

// -----------------------------------------------------------------------------
// Mutable Global Options
// -----------------------------------------------------------------------------
/** test mode (turn it on only when executing tests) */
var TEST_MODE: Boolean = false
