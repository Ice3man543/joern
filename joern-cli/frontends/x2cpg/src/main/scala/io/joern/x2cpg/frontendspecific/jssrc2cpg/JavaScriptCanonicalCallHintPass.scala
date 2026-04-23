package io.joern.x2cpg.frontendspecific.jssrc2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.importresolver.{EvaluatedImport, UnknownMethod, UnknownTypeDecl}

import scala.util.matching.Regex

/** For calls whose receiver resolves to an external npm package `P`, emit a canonical dotted
  * `"$P.$M"` hint on [[Call.dynamicTypeHintFullName]] so that downstream rule catalogues
  * matching against the idiomatic dotted form (`".*sequelize\\.query.*"`) light up.
  *
  * `methodFullName` is never changed — it keeps carrying jssrc2cpg's internal representation
  * (`<member>(X):Y`, `pkg:Cls:method`, …) for consumers that parse it.
  *
  * This pass runs AFTER [[JavaScriptTypeRecovery]] and [[JavaScriptTypeHintCallLinker]] so that
  * any final `methodFullName` rewrites those passes perform are already visible here.
  *
  * Two fullName shapes are recognised:
  *   - direct form: `"$P:...:$M"` — first `:`-separated segment is the package name;
  *   - indirect form: `".*<member>($P):$M"` — used by the recovery pass for multi-hop field
  *     access like `models.sequelize.query` in juice-shop.
  *
  * In both cases we source the method token from the call's own `name` property (more reliable
  * than regex-extracting) and require the leading / `<member>(…)` token to be one of the
  * external package names harvested from unresolved-import tags.
  */
class JavaScriptCanonicalCallHintPass(cpg: Cpg) extends CpgPass(cpg) {

  private val pathSep: String = ":"

  /** `.*<member>(P):M$` — internal multi-hop form. */
  private val MemberSuffixRe: Regex = ".*<member>\\(([^)]+)\\):([^:(]+)$".r

  override def run(builder: DiffGraphBuilder): Unit = {
    val externalPackageNames = harvestExternalPackageNames()
    if (externalPackageNames.isEmpty) return

    cpg.call.iterator.foreach(canonicaliseCall(_, externalPackageNames, builder))
  }

  private def harvestExternalPackageNames(): Set[String] = {
    def looksLikeBarePackage(s: String): Boolean =
      s.nonEmpty && !s.contains('/') && !s.contains(java.io.File.separatorChar) && !s.contains('.')

    cpg.imports.iterator
      .flatMap(_.call.iterator.flatMap(_.tag))
      .flatMap(EvaluatedImport.tagToEvaluatedImport)
      .collect {
        case UnknownMethod(fullName, _, _, _) => fullName
        case UnknownTypeDecl(fullName, _)     => fullName
      }
      .map(_.split(pathSep).head)
      .filter(looksLikeBarePackage)
      .toSet
  }

  private def canonicaliseCall(call: Call, externalPackageNames: Set[String], builder: DiffGraphBuilder): Unit = {
    val method = call.name
    if (method.isEmpty || method.startsWith("<operator") || method.startsWith("<member>")) return

    val sources  = call.methodFullName +: call.dynamicTypeHintFullName.toList
    val existing = call.dynamicTypeHintFullName.toSet

    val canonical = sources.iterator.flatMap { s =>
      val viaMember = s match {
        case MemberSuffixRe(pkg, m) if externalPackageNames.contains(pkg) => Some(s"$pkg.$m")
        case _                                                            => None
      }
      val viaPrefix = s.split(pathSep).headOption match {
        case Some(head) if externalPackageNames.contains(head) && head != s =>
          Some(s"$head.$method")
        case _ => None
      }
      viaMember.iterator ++ viaPrefix.iterator
    }.toSet

    val toAdd = canonical.diff(existing)
    if (toAdd.nonEmpty) {
      builder.setNodeProperty(
        call,
        PropertyNames.DynamicTypeHintFullName,
        (call.dynamicTypeHintFullName ++ toAdd).distinct
      )
    }
  }

}
