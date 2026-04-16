package io.joern.x2cpg.frontendspecific.gosrc2cpg

import io.joern.x2cpg.passes.frontend.XTypeHintCallLinker
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class GoTypeHintCallLinker(cpg: Cpg) extends XTypeHintCallLinker(cpg) {

  override def calls: Iterator[Call] = super.calls

  override def calleeNames(c: Call): Seq[String] = super.calleeNames(c)
}
