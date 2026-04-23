package io.joern.x2cpg.frontendspecific.jssrc2cpg

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.testing.MockCpg
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ObjectPropertyCallLinkerTests extends AnyWordSpec with Matchers {

  private val fileName   = "test.js"
  private val methodName = "program"

  private def baseCpg(): MockCpg =
    MockCpg()
      .withFile(fileName)
      .withMethod(methodName, fileName = fileName)

  "ObjectPropertyCallLinker" should {

    "not throw when the graph contains an assignment with more than two arguments" in {
      val cpg = baseCpg()
        .withCallInMethod(methodName, Operators.assignment, Some("a = b = c"))
        .withIdentifierArgument(Operators.assignment, "a", 1)
        .withIdentifierArgument(Operators.assignment, "b", 2)
        .withIdentifierArgument(Operators.assignment, "c", 3)
        .cpg

      noException should be thrownBy new ObjectPropertyCallLinker(cpg).createAndApply()
    }

    "iterate over well-formed two-argument assignments even when a malformed assignment is present" in {
      val cpg = baseCpg()
        .withCallInMethod(methodName, Operators.assignment, Some("a = b = c"))
        .withIdentifierArgument(Operators.assignment, "a", 1)
        .withIdentifierArgument(Operators.assignment, "b", 2)
        .withIdentifierArgument(Operators.assignment, "c", 3)
        .withMethod("handler", fileName = fileName)
        .withCustom { (graph, cpg) =>
          val method        = cpg.method.nameExact(methodName).head
          val block         = method.block
          val handlerMethod = cpg.method.nameExact("handler").head

          val goodAssignment = NewCall()
            .name(Operators.assignment)
            .methodFullName(Operators.assignment)
            .code("obj.foo = handler")
          val fieldAccess = NewCall()
            .name(Operators.fieldAccess)
            .methodFullName(Operators.fieldAccess)
            .code("obj.foo")
            .argumentIndex(1)
          val methodRef = NewMethodRef()
            .methodFullName(s"$fileName::program:handler")
            .code("handler")
            .argumentIndex(2)

          graph.addNode(goodAssignment)
          graph.addNode(fieldAccess)
          graph.addNode(methodRef)
          graph.addEdge(methodRef, handlerMethod, EdgeTypes.REF)
          graph.addEdge(block, goodAssignment, EdgeTypes.AST)
          graph.addEdge(method, goodAssignment, EdgeTypes.CONTAINS)
          graph.addEdge(goodAssignment, fieldAccess, EdgeTypes.AST)
          graph.addEdge(goodAssignment, fieldAccess, EdgeTypes.ARGUMENT)
          graph.addEdge(goodAssignment, methodRef, EdgeTypes.AST)
          graph.addEdge(goodAssignment, methodRef, EdgeTypes.ARGUMENT)
          graph.addEdge(method, fieldAccess, EdgeTypes.CONTAINS)
          graph.addEdge(method, methodRef, EdgeTypes.CONTAINS)
        }
        .cpg

      noException should be thrownBy new ObjectPropertyCallLinker(cpg).createAndApply()

      cpg.assignment.size shouldBe 2
      val twoArgAssignments = cpg.assignment.filter(_.argument.size == 2).l
      twoArgAssignments should have size 1
      twoArgAssignments.head.source shouldBe a[MethodRef]
    }
  }
}
