package io.joern.x2cpg.frontendspecific.gosrc2cpg

import io.joern.x2cpg.passes.frontend.{RecoverForXCompilationUnit, XTypeRecovery, XTypeRecoveryState}
import io.shiftleft.codepropertygraph.generated.{Cpg, DiffGraphBuilder}
import io.shiftleft.codepropertygraph.generated.nodes.File
import io.shiftleft.semanticcpg.language.*

private class GoTypeRecovery(cpg: Cpg, state: XTypeRecoveryState, iteration: Int)
    extends XTypeRecovery[File](cpg, state, iteration) {

  override def compilationUnits: Iterator[File] = cpg.file.iterator

  override def generateRecoveryForCompilationUnitTask(
    unit: File,
    builder: DiffGraphBuilder
  ): RecoverForXCompilationUnit[File] = {
    new RecoverForGoFile(cpg, unit, builder, state)
  }
}
