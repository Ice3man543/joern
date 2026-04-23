package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import org.slf4j.LoggerFactory

import java.util.concurrent.atomic.AtomicBoolean

object AssignmentMethods {
  private val logger     = LoggerFactory.getLogger(getClass)
  private val warnedOnce = new AtomicBoolean(false)
}

class AssignmentMethods(val assignment: OpNodes.Assignment) extends AnyVal {

  def target: Expression = assignment.argument(1)

  def source: Expression = {
    assignment.argument.size match {
      case 0 =>
        throw new RuntimeException(s"Assignment with 0 arguments at ${assignment.code}")
      case 1 => assignment.argument(1)
      case 2 => assignment.argument(2)
      case n =>
        if (AssignmentMethods.warnedOnce.compareAndSet(false, true)) {
          AssignmentMethods.logger.warn(
            s"Assignment statement with $n arguments (expected <= 2); returning last argument. code='${assignment.code}'"
          )
        }
        assignment.argument(n)
    }
  }
}
