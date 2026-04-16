package io.joern.x2cpg.frontendspecific

import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.CpgPassBase

package object gosrc2cpg {

  def postProcessingPasses(cpg: Cpg, typeRecoveryConfig: XTypeRecoveryConfig): List[CpgPassBase] = {
    new GoTypeRecoveryPassGenerator(cpg, typeRecoveryConfig).generate()
      ++ List(
        new AstLinkerPass(cpg)
      )
  }
}
