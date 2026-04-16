package io.joern.go2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.semanticcpg.language.*

/** Validation tests for Go taint analysis fixes.
  *
  * Tests validate:
  *   1. CALL edges exist after StaticCallLinker (Phase 1)
  *   2. Interprocedural dataflow across user-defined functions
  *   3. Cross-package taint propagation
  *   4. Struct method taint propagation
  *   5. Go flow semantics for stdlib (Phase 2)
  */
class GoTaintAnalysisValidationTests extends GoCodeToCpgSuite(withOssDataflow = true) {

  // ==================== Phase 1: CALL edge + basic interprocedural flow ====================

  "Interprocedural taint: literal -> function -> identifier" should {
    val cpg = code("""
        |package main
        |func bar(x int) int {
        |  return x
        |}
        |func main() {
        |  var a = 10
        |  var b = bar(a)
        |  println(b)
        |}
        |""".stripMargin)

    "track flow from identifier through function call to println" in {
      val src  = cpg.identifier("a").lineNumber(7).l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "track flow from literal through function call to println sink" in {
      val src  = cpg.literal("10").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  // ==================== Multi-hop interprocedural ====================

  "Multi-hop interprocedural taint" should {
    val cpg = code("""
        |package main
        |func process(input string) string {
        |  return input
        |}
        |func wrap(data string) string {
        |  return process(data)
        |}
        |func main() {
        |  var userInput = "tainted"
        |  var result = wrap(userInput)
        |  println(result)
        |}
        |""".stripMargin)

    "track taint: literal -> wrap -> process -> result -> println" in {
      val src  = cpg.literal("\"tainted\"").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "track taint from identifier to println" in {
      val src  = cpg.identifier("userInput").lineNumber(10).l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  // ==================== Command injection ====================

  "Command injection: string concat into exec.Command" should {
    val cpg = code("""
        |package main
        |import "os/exec"
        |func runCommand(userInput string) {
        |  cmd := exec.Command("sh", "-c", userInput)
        |  cmd.Run()
        |}
        |""".stripMargin)

    "track taint from parameter to exec.Command arg within same function" in {
      val src  = cpg.method.name("runCommand").parameter.name("userInput").l
      val sink = cpg.call.name("Command").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  // ==================== SQL injection pattern ====================

  "SQL injection: string concat into query" should {
    val cpg = code("""
        |package main
        |func buildAndRun(input string) {
        |  query := "SELECT * FROM users WHERE name = " + input
        |  println(query)
        |}
        |func main() {
        |  var userInput = "malicious"
        |  buildAndRun(userInput)
        |}
        |""".stripMargin)

    "track taint from parameter through concat to println (intra-procedural)" in {
      val src  = cpg.method.name("buildAndRun").parameter.name("input").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }

    "track taint from literal through function to println (inter-procedural)" in {
      val src  = cpg.literal("\"malicious\"").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  // ==================== Cross-package taint ====================

  "Cross-package taint propagation" should {
    val cpg = code(
      """
        |module joern.io/sample
        |go 1.18
        |""".stripMargin,
      "go.mod"
    ).moreCode(
      """
        |package lib
        |func ProcessInput(data string) string {
        |  return data
        |}
        |""".stripMargin,
      "lib/process.go"
    ).moreCode(
      """
        |package main
        |import "joern.io/sample/lib"
        |func main() {
        |  userInput := "tainted"
        |  result := lib.ProcessInput(userInput)
        |  println(result)
        |}
        |""".stripMargin,
      "main.go"
    )

    "track taint from literal across package boundary to println" in {
      val src  = cpg.literal("\"tainted\"").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  // ==================== Struct method chain ====================

  "Struct method taint propagation" should {
    val cpg = code("""
        |package main
        |type Person struct {
        |  fname string
        |  lname string
        |}
        |func (person Person) fullName() string {
        |  return person.fname + " " + person.lname
        |}
        |func main() {
        |  var a = Person{fname: "Pandurang", lname: "Patil"}
        |  var fullname string = a.fullName()
        |  println(fullname)
        |}
        |""".stripMargin)

    "track taint from struct literal through method to println" in {
      val src  = cpg.literal("\"Pandurang\"").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size should be >= 1
    }
  }

  // ==================== String building pattern ====================

  "String building taint chain" should {
    val cpg = code("""
        |package main
        |func buildQuery(input string) string {
        |  return "SELECT * FROM users WHERE id = " + input
        |}
        |func main() {
        |  var userInput = "1 OR 1=1"
        |  var query = buildQuery(userInput)
        |  println(query)
        |}
        |""".stripMargin)

    "track taint from literal through string building to println" in {
      val src  = cpg.literal("\"1 OR 1=1\"").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size shouldBe 1
    }
  }

  // ==================== Receiver field chain (vuln-go pattern) ====================

  "Receiver field chain: recv.field.Method()" should {
    val cpg = code("""
        |package main
        |import "net/http"
        |import "os/exec"
        |type httpHelper struct {
        |  r *http.Request
        |}
        |func (h httpHelper) GetQueryParam(key string) string {
        |  return h.r.URL.Query().Get(key)
        |}
        |func getTime(w http.ResponseWriter, r *http.Request) {
        |  h := httpHelper{r: r}
        |  format := h.GetQueryParam("format")
        |  cmd := exec.Command("date", "+"+format)
        |  cmd.Run()
        |}
        |""".stripMargin)

    "track taint: http.Request param -> h.r.URL.Query().Get -> Command (inter-procedural)" in {
      val src  = cpg.method.name("getTime").parameter.name("r").l
      val sink = cpg.call.name("Command").l
      sink.reachableByFlows(src).size should be >= 1
    }

    "intra: h param -> h.r.URL.Query() inside GetQueryParam" in {
      val src  = cpg.method.name("GetQueryParam").parameter.name("h").l
      val sink = cpg.call.name("Query").l
      sink.reachableByFlows(src).size should be >= 1
    }

    "intra: h param -> h.r inside GetQueryParam" in {
      val src = cpg.method.name("GetQueryParam").parameter.name("h").l
      // fieldAccess for h.r
      val sink = cpg.call.name("<operator>.fieldAccess").code("h.r").l
      sink.reachableByFlows(src).size should be >= 1
    }

    "intra: h param -> h.r.URL fieldAccess inside GetQueryParam" in {
      val src  = cpg.method.name("GetQueryParam").parameter.name("h").l
      val sink = cpg.call.name("<operator>.fieldAccess").code("h.r.URL").l
      sink.reachableByFlows(src).size should be >= 1
    }

    "intra: Get call to its return inside GetQueryParam" in {
      val src  = cpg.method.name("GetQueryParam").parameter.name("h").l
      val sink = cpg.call.name("Get").l
      sink.reachableByFlows(src).size should be >= 1
    }
  }

  // ==================== Multiple return values ====================

  "Go multiple return values" should {
    val cpg = code("""
        |package main
        |func getData() (string, int) {
        |  return "tainted", 42
        |}
        |func main() {
        |  var data, _ = getData()
        |  println(data)
        |}
        |""".stripMargin)

    "track taint from literal in multi-return to println" in {
      val src  = cpg.literal("\"tainted\"").l
      val sink = cpg.call("println").l
      sink.reachableByFlows(src).size should be >= 1
    }
  }
}
