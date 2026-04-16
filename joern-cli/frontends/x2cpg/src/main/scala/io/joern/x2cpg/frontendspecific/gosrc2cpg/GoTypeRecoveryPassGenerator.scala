package io.joern.x2cpg.frontendspecific.gosrc2cpg

import io.joern.x2cpg.passes.frontend.{
  XTypeRecovery,
  XTypeRecoveryConfig,
  XTypeRecoveryPassGenerator,
  XTypeRecoveryState
}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.File

class GoTypeRecoveryPassGenerator(cpg: Cpg, config: XTypeRecoveryConfig = XTypeRecoveryConfig())
    extends XTypeRecoveryPassGenerator[File](cpg, config) {

  override protected def generateRecoveryPass(state: XTypeRecoveryState, iteration: Int): XTypeRecovery[File] =
    new GoTypeRecovery(cpg, state, iteration)
}
