package io.joern.jssrc2cpg.utils

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern
import scala.util.Try
import scala.util.matching.Regex

/** Single source of truth for jssrc2cpg default file/path exclusions.
  *
  * The directory regexes match anywhere in the relative path (so nested `vendor/` or `dist/`
  * directories are also excluded). File and test regexes match on the full relative path.
  *
  * The bundled-content heuristic reads at most the first 64 KiB of a file and flags it as
  * a bundled/minified JavaScript asset when either the longest line exceeds `MaxLineLength`
  * chars or the average line length exceeds `AvgLineLength` chars. It exists because files
  * like the 38k-line `three.js` bundle are neither matched by a directory exclude nor by
  * the legacy `*.min.js` / `*.bundle.js` filename patterns.
  */
object DefaultExcludedPaths {

  private val Sep: String = Pattern.quote(java.io.File.separator)

  private def anywhereDir(name: String): Regex =
    s"(?:.*$Sep)?${Pattern.quote(name)}$Sep.*".r

  /** Directory components excluded anywhere in the relative path. */
  val DirectoryRegex: Seq[Regex] = Seq(
    "node_modules",
    "venv",
    "docs",
    "test",
    "tests",
    "e2e",
    "e2e-beta",
    "examples",
    "cypress",
    "jest-cache",
    "eslint-rules",
    "codemods",
    "flow-typed",
    "i18n",
    "vendor",
    "www",
    "dist",
    "build",
    "out",
    "target",
    "coverage",
    ".next",
    ".nuxt",
    ".svelte-kit"
  ).map(anywhereDir)

  /** Generic extension/name pattern (mirrors the historic astgen-side default). */
  val ExtensionRegex: Seq[Regex] = Seq(
    "(.*/)?(conf|test|spec|[.-]min|\\.d)\\.(js|jsx|cjs|mjs|xsjs|xsjslib|ts|tsx)$".r
  )

  val TestRegex: Seq[Regex] = Seq(
    ".*[.-]spec\\.js".r,
    ".*[.-]mock\\.js".r,
    ".*[.-]e2e\\.js".r,
    ".*[.-]test\\.js".r,
    ".*cypress\\.json".r,
    ".*test.*\\.json".r
  )

  val FileRegex: Seq[Regex] = Seq(
    ".*jest\\.config.*".r,
    ".*webpack\\..*\\.js".r,
    ".*vue\\.config\\.js".r,
    ".*babel\\.config\\.js".r,
    ".*chunk-vendors.*\\.js".r,
    ".*app~.*\\.js".r,
    ".*\\.chunk\\.js".r,
    ".*\\.babelrc.*".r,
    ".*\\.eslint.*".r,
    ".*\\.tslint.*".r,
    ".*\\.stylelintrc\\.js".r,
    ".*rollup\\.config.*".r,
    ".*\\.types\\.js".r,
    ".*\\.cjs\\.js".r,
    ".*eslint-local-rules\\.js".r,
    ".*\\.devcontainer\\.json".r,
    ".*Gruntfile\\.js".r,
    ".*i18n.*\\.json".r,
    ".*\\.min\\.(js|mjs|cjs)$".r,
    ".*\\.bundle\\.js$".r
  )

  /** Combined default exclusion regexes passed to `SourceFiles.determine`. */
  val All: Seq[Regex] = DirectoryRegex ++ ExtensionRegex ++ TestRegex ++ FileRegex

  /** Legacy filename-only minified pattern retained for backwards-compatible shortcuts. */
  val MinifiedPathRegex: Regex = ".*([.-]min\\..*js|bundle\\.js)".r

  private val MaxBytesSampled: Int   = 64 * 1024
  private val MaxLineLength: Int     = 5000
  private val AvgLineLength: Int     = 200
  /** Files larger than this (in bytes) are assumed to be bundled/library code
    * that is not worth analysing as user source. Hand-written jssrc2cpg-scope
    * modules practically never exceed this.
    */
  private val LargeBundleFileBytes: Long = 512L * 1024L
  private val JsLikeExtensions           = Set(".js", ".mjs", ".cjs")

  private def extensionOf(path: Path): Option[String] = {
    val name = path.getFileName.toString
    val dot  = name.lastIndexOf('.')
    if (dot < 0) None else Some(name.substring(dot).toLowerCase)
  }

  private def sampleLineStats(path: Path): Option[(Int, Double)] = Try {
    val buf = new Array[Byte](MaxBytesSampled)
    val in  = Files.newInputStream(path)
    try {
      var total = 0
      var n     = 0
      while (total < buf.length && { n = in.read(buf, total, buf.length - total); n > 0 }) total += n
      if (total == 0) None
      else {
        val text  = new String(buf, 0, total, StandardCharsets.UTF_8)
        val lines = text.split('\n')
        val complete =
          if (total == MaxBytesSampled && lines.length > 1) lines.init else lines
        if (complete.isEmpty) None
        else {
          val max = complete.iterator.map(_.length).max
          val avg = complete.iterator.map(_.length.toDouble).sum / complete.length
          Some((max, avg))
        }
      }
    } finally in.close()
  }.toOption.flatten

  /** Heuristic: is this `.js` / `.mjs` / `.cjs` file a bundled/library asset?
    *
    * Two signals, either one is sufficient:
    *   1. File size exceeds [[LargeBundleFileBytes]] — hand-written app modules
    *      practically never reach this; catches non-minified library bundles
    *      (e.g. `three.js`).
    *   2. Reads at most the first 64 KiB and checks whether the longest line
    *      exceeds [[MaxLineLength]] or the average line length exceeds
    *      [[AvgLineLength]] — catches minified bundles.
    */
  def isLikelyBundledJs(filePath: String): Boolean = {
    val path = Paths.get(filePath)
    val ext  = extensionOf(path).getOrElse("")
    if (!JsLikeExtensions.contains(ext) || !Files.isRegularFile(path)) return false
    val sizeHit = Try(Files.size(path)).toOption.exists(_ > LargeBundleFileBytes)
    if (sizeHit) return true
    sampleLineStats(path).exists { case (maxLen, avgLen) =>
      maxLen > MaxLineLength || avgLen > AvgLineLength
    }
  }
}
