package io.joern.x2cpg.frontendspecific.gosrc2cpg

import io.joern.x2cpg.Defines
import io.joern.x2cpg.passes.frontend.*
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.FieldAccess

/** Performs type recovery for a single Go compilation unit (file). */
private class RecoverForGoFile(cpg: Cpg, cu: File, builder: DiffGraphBuilder, state: XTypeRecoveryState)
    extends RecoverForXCompilationUnit[File](cpg, cu, builder, state) {

  /** Go uses "." as the path separator for qualified names (e.g., "net/http.Request.FormValue"). */
  override protected val pathSep: String = "."

  /** Go has no explicit constructors. We use the convention that factory functions
    * named "New*" (e.g., NewServer, NewReader) act as constructors.
    */
  override def isConstructor(c: Call): Boolean =
    isConstructor(c.name)

  override protected def isConstructor(name: String): Boolean =
    name.nonEmpty && name.startsWith("New") && name.length > 3 && name.charAt(3).isUpper

  /** Go does not have self/this. Method receivers are named parameters,
    * so the default SBKey mapping works correctly.
    */
  override protected def fromNodeToLocalKey(node: AstNode): Option[LocalKey] =
    SBKey.fromNodeToLocalKey(node)

  /** Map Go literal values to their types. Only infers types for literals that don't already
    * have a known type set by the frontend.
    */
  override def getLiteralType(l: Literal): Set[String] = {
    // If the frontend already resolved a specific type, preserve it
    val existingType = l.typeFullName
    if (existingType != null && existingType.nonEmpty && existingType != "ANY" && existingType != Defines.Any) {
      return Set(existingType)
    }
    val literalTypes = (l.code match {
      case code if code.toIntOption.isDefined                                   => Some("int")
      case code if code.toDoubleOption.isDefined                                => Some("float64")
      case code if code == "true" || code == "false"                            => Some("bool")
      case code if code == "nil"                                                => Some(Defines.Any)
      case code if code.startsWith("\"") || code.startsWith("`")               => Some("string")
      case code if code.startsWith("'")                                         => Some("rune")
      case _                                                                    => None
    }).toSet
    setTypes(l, literalTypes.toSeq)
    literalTypes
  }

  override def visitIdentifierAssignedToOperator(i: Identifier, c: Call, operation: String): Set[String] = {
    operation match {
      case Operators.indexAccess =>
        c.argument.argumentIndex(1).isCall.foreach(setCallMethodFullNameFromBase)
        visitIdentifierAssignedToIndexAccess(i, c)
      case _ => super.visitIdentifierAssignedToOperator(i, c, operation)
    }
  }

  /** When a constructor like NewServer() is called, the returned type is the type itself
    * (without the "New" prefix and constructor suffix).
    */
  override def visitIdentifierAssignedToConstructor(i: Identifier, c: Call): Set[String] = {
    val constructorPaths = symbolTable.get(c).map { path =>
      // Strip the constructor method name to get the type, e.g., "pkg.NewServer" -> "pkg.Server"
      val parts = path.split("\\.")
      if (parts.length >= 2) {
        val methodName = parts.last
        val typeName   = if (methodName.startsWith("New")) methodName.stripPrefix("New") else methodName
        parts.init.mkString(".") + "." + typeName
      } else {
        path
      }
    }
    associateTypes(i, constructorPaths)
  }

  override def visitIdentifierAssignedToCall(i: Identifier, c: Call): Set[String] = {
    super.visitIdentifierAssignedToCall(i, c)
  }

  override def visitIdentifierAssignedToFieldLoad(i: Identifier, fa: FieldAccess): Set[String] = {
    val fieldParents = getFieldParents(fa)
    if (fieldParents.nonEmpty) {
      fa.astChildren.l match {
        case List(_: Identifier, fi: FieldIdentifier) =>
          val referencedFields = cpg.typeDecl.fullNameExact(fieldParents.toSeq*).member.nameExact(fi.canonicalName)
          val globalTypes =
            referencedFields.flatMap(m => m.typeFullName +: m.dynamicTypeHintFullName).filterNot(_ == "ANY").toSet
          if (globalTypes.nonEmpty) associateTypes(i, globalTypes)
          else super.visitIdentifierAssignedToFieldLoad(i, fa)
        case _ => super.visitIdentifierAssignedToFieldLoad(i, fa)
      }
    } else {
      super.visitIdentifierAssignedToFieldLoad(i, fa)
    }
  }

  override def getTypesFromCall(c: Call): Set[String] = c.name match {
    case "<operator>.arrayInitializer" => Set("[]any")
    case _                             => super.getTypesFromCall(c)
  }
}
