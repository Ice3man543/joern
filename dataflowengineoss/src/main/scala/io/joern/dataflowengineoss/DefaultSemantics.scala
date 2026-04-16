package io.joern.dataflowengineoss

import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, FullNameSemantics}
import io.joern.dataflowengineoss.semanticsloader.FlowPath.{FlowMapping, PassThroughMapping}
import io.shiftleft.codepropertygraph.generated.Operators

import scala.annotation.unused

object DefaultSemantics {

  /** @return
    *   a default set of common external procedure calls for all languages.
    */
  def apply(): FullNameSemantics = {
    val list = operatorFlows ++ cFlows ++ javaFlows ++ goFlows
    FullNameSemantics.fromList(list)
  }

  private def F = (x: String, y: List[(Int, Int)]) => FlowSemantic.from(x, y)

  private def PTF(x: String, ys: List[(Int, Int)] = List.empty): FlowSemantic =
    FlowSemantic(x).copy(mappings = FlowSemantic.from(x, ys).mappings :+ PassThroughMapping)

  def operatorFlows: List[FlowSemantic] = List(
    F(Operators.addition, List((1, -1), (2, -1))),
    F(Operators.addressOf, List((1, -1))),
    F(Operators.assignment, List((2, 1), (2, -1))),
    F(Operators.assignmentAnd, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentArithmeticShiftRight, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentDivision, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentExponentiation, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentLogicalShiftRight, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentMinus, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentModulo, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentMultiplication, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentOr, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentPlus, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentShiftLeft, List((2, 1), (1, 1), (2, -1))),
    F(Operators.assignmentXor, List((2, 1), (1, 1), (2, -1))),
    F(Operators.cast, List((1, -1), (2, -1))),
    F(Operators.computedMemberAccess, List((1, -1))),
    F(Operators.conditional, List((2, -1), (3, -1))),
    F(Operators.elvis, List((1, -1), (2, -1))),
    F(Operators.notNullAssert, List((1, -1))),
    F(Operators.fieldAccess, List((1, -1))),
    F(Operators.getElementPtr, List((1, -1))),
    PTF(Operators.modulo, List.empty),
    PTF(Operators.arrayInitializer, List.empty),

    // TODO does this still exist?
    F("<operator>.incBy", List((1, 1), (2, 1), (3, 1), (4, 1))),
    F(Operators.indexAccess, List((1, -1))),
    F(Operators.indirectComputedMemberAccess, List((1, -1))),
    F(Operators.indirectFieldAccess, List((1, -1))),
    F(Operators.indirectIndexAccess, List((1, -1), (2, 1))),
    F(Operators.indirectMemberAccess, List((1, -1))),
    F(Operators.indirection, List((1, -1))),
    F(Operators.memberAccess, List((1, -1))),
    F(Operators.pointerShift, List((1, -1))),
    F(Operators.postDecrement, List((1, 1), (1, -1))),
    F(Operators.postIncrement, List((1, 1), (1, -1))),
    F(Operators.preDecrement, List((1, 1), (1, -1))),
    F(Operators.preIncrement, List((1, 1), (1, -1))),
    F(Operators.sizeOf, List.empty[(Int, Int)]),

    // Language specific operators
    PTF("<operator>.tupleLiteral"),
    PTF("<operator>.dictLiteral"),
    PTF("<operator>.setLiteral"),
    PTF("<operator>.listLiteral")
  )

  /** Semantic summaries for common external C/C++ calls.
    *
    * @see
    *   <a href="https://www.ibm.com/docs/en/i/7.3?topic=extensions-standard-c-library-functions-table-by-name">Standard
    *   C Library Functions</a>
    */
  def cFlows: List[FlowSemantic] = List(
    F("abs", List((1, 1), (1, -1))),
    F("abort", List.empty[(Int, Int)]),
    F("asctime", List((1, 1), (1, -1))),
    F("asctime_r", List((1, 1), (1, -1))),
    F("atof", List((1, 1), (1, -1))),
    F("atoi", List((1, 1), (1, -1))),
    F("atol", List((1, 1), (1, -1))),
    F("calloc", List((1, -1), (2, -1))),
    F("ceil", List((1, 1), (1, 1))),
    F("clock", List.empty[(Int, Int)]),
    F("ctime", List((1, -1))),
    F("ctime64", List((1, -1))),
    F("ctime_r", List((1, -1))),
    F("ctime64_r", List((1, -1))),
    F("difftime", List((1, -1), (2, -1))),
    F("difftime64", List((1, -1), (2, -1))),
    PTF("div"),
    F("exit", List((1, 1))),
    F("exp", List((1, -1))),
    F("fabs", List((1, -1))),
    F("fclose", List((1, 1), (1, -1))),
    F("fdopen", List((1, -1), (2, -1))),
    F("feof", List((1, 1), (1, -1))),
    F("ferror", List((1, 1), (1, -1))),
    F("fflush", List((1, 1), (1, -1))),
    F("fgetc", List((1, 1), (1, -1))),
    F("fwrite", List((1, 1), (1, -1), (2, -1), (3, -1), (4, -1))),
    F("free", List((1, 1))),
    F("getc", List((1, 1))),
    F("scanf", List((2, 2))),
    F("strcmp", List((1, 1), (1, -1), (2, 2), (2, -1))),
    F("strlen", List((1, 1), (1, -1))),
    F("strncpy", List((1, 1), (2, 2), (3, 3), (1, -1), (2, -1))),
    F("strncat", List((1, 1), (2, 2), (3, 3), (1, -1), (2, -1)))
  )

  /** Semantic summaries for common external Java calls.
    */
  def javaFlows: List[FlowSemantic] = List(
    PTF("java.lang.String.split:java.lang.String[](java.lang.String)", List((0, 0))),
    PTF("java.lang.String.split:java.lang.String[](java.lang.String,int)", List((0, 0))),
    PTF("java.lang.String.compareTo:int(java.lang.String)", List((0, 0))),
    F("java.io.PrintWriter.print:void(java.lang.String)", List((0, 0), (1, 1))),
    F("java.io.PrintWriter.println:void(java.lang.String)", List((0, 0), (1, 1))),
    F("java.io.PrintStream.println:void(java.lang.String)", List((0, 0), (1, 1))),
    PTF("java.io.PrintStream.print:void(java.lang.String)", List((0, 0))),
    F("android.text.TextUtils.isEmpty:boolean(java.lang.String)", List((0, -1), (1, -1))),
    F("java.sql.PreparedStatement.prepareStatement:java.sql.PreparedStatement(java.lang.String)", List((1, -1))),
    F("java.sql.PreparedStatement.prepareStatement:setDouble(int,double)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setFloat(int,float)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setInt(int,int)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setLong(int,long)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setShort(int,short)", List((1, 1), (2, 2))),
    F("java.sql.PreparedStatement.prepareStatement:setString(int,java.lang.String)", List((1, 1), (2, 2))),
    F("org.apache.http.HttpRequest.<init>:void(org.apache.http.RequestLine)", List((1, 1), (1, 0))),
    F("org.apache.http.HttpRequest.<init>:void(java.lang.String,java.lang.String)", List((1, 1), (1, 0), (2, 0))),
    F(
      "org.apache.http.HttpRequest.<init>:void(java.lang.String,java.lang.String,org.apache.http.ProtocolVersion)",
      List((1, 1), (1, 0), (2, 2), (2, 0), (3, 3), (3, 0))
    ),
    F("org.apache.http.HttpResponse.getStatusLine:org.apache.http.StatusLine()", List((0, -1))),
    F("org.apache.http.HttpResponse.setStatusLine:void(org.apache.http.StatusLine)", List((1, 0), (1, 1), (0, -1))),
    F("org.apache.http.HttpResponse.setReasonPhrase:void(java.lang.String)", List((1, 0), (1, 1), (0, -1))),
    F("org.apache.http.HttpResponse.getEntity:org.apache.http.HttpEntity()", List((0, -1))),
    F("org.apache.http.HttpResponse.setEntity:void(org.apache.http.HttpEntity)", List((1, 0), (1, 1), (1, 0)))
  )

  /** Semantic summaries for common external Go stdlib calls.
    *
    * Go method full names in Joern follow the pattern:
    *   - Package-level functions: "package/path.FuncName" (e.g., "fmt.Sprintf")
    *   - Struct methods: "package/path.TypeName.MethodName" (e.g., "net/http.Request.FormValue")
    *
    * Index 0 = receiver (for struct methods), positive = arg position, -1 = return value.
    */
  def goFlows: List[FlowSemantic] = List(
    // === fmt package: string formatting ===
    PTF("fmt.Sprintf"),
    PTF("fmt.Sprint"),
    PTF("fmt.Sprintln"),
    F("fmt.Fprintf", List((1, 1), (2, 1), (2, -1))),
    PTF("fmt.Printf"),
    PTF("fmt.Println"),
    PTF("fmt.Print"),
    F("fmt.Fprint", List((1, 1), (2, 1), (2, -1))),
    F("fmt.Fprintln", List((1, 1), (2, 1), (2, -1))),
    F("fmt.Sscanf", List((1, -1), (1, 3))),
    F("fmt.Sscan", List((1, -1), (1, 2))),
    F("fmt.Errorf", List((1, -1))),
    // === net/http package: HTTP request/response ===
    F("net/http.Request.FormValue", List((0, -1), (1, -1))),
    F("net/http.Request.PostFormValue", List((0, -1), (1, -1))),
    F("net/http.Request.Header.Get", List((0, -1), (1, -1))),
    F("net/http.Request.URL.Query", List((0, -1))),
    F("net/http.Request.Cookie", List((0, -1), (1, -1))),
    F("net/http.Request.Cookies", List((0, -1))),
    F("net/http.Request.Referer", List((0, -1))),
    F("net/http.Request.UserAgent", List((0, -1))),
    F("net/http.Request.Body", List((0, -1))),
    F("net/http.ResponseWriter.Write", List((0, 0), (1, 0), (1, -1))),
    F("net/http.ResponseWriter.WriteHeader", List((0, 0), (1, 0))),
    F("net/http.ResponseWriter.Header", List((0, -1))),
    // === net/url package: URL parsing ===
    F("net/url.URL.Query", List((0, -1))),
    F("net/url.URL.String", List((0, -1))),
    F("net/url.Values.Get", List((0, -1), (1, -1))),
    F("net/url.Values.Set", List((1, 0), (2, 0))),
    F("net/url.Values.Add", List((1, 0), (2, 0))),
    F("net/url.Values.Encode", List((0, -1))),
    F("net/url.Parse", List((1, -1))),
    F("net/url.QueryEscape", List((1, -1))),
    F("net/url.QueryUnescape", List((1, -1))),
    F("net/url.PathEscape", List((1, -1))),
    F("net/url.PathUnescape", List((1, -1))),
    // === database/sql: SQL injection vectors ===
    F("database/sql.DB.Query", List((1, -1), (2, -1))),
    F("database/sql.DB.QueryRow", List((1, -1), (2, -1))),
    F("database/sql.DB.Exec", List((1, -1), (2, -1))),
    F("database/sql.DB.QueryContext", List((2, -1), (3, -1))),
    F("database/sql.DB.ExecContext", List((2, -1), (3, -1))),
    F("database/sql.DB.Prepare", List((1, -1))),
    F("database/sql.Tx.Query", List((1, -1), (2, -1))),
    F("database/sql.Tx.QueryRow", List((1, -1), (2, -1))),
    F("database/sql.Tx.Exec", List((1, -1), (2, -1))),
    F("database/sql.Stmt.Query", List((1, -1))),
    F("database/sql.Stmt.QueryRow", List((1, -1))),
    F("database/sql.Stmt.Exec", List((1, -1))),
    F("database/sql.Row.Scan", List((0, 1))),
    F("database/sql.Rows.Scan", List((0, 1))),
    // === os/exec: command injection vectors (variadic) ===
    PTF("os/exec.Command"),
    PTF("os/exec.CommandContext"),
    F("os/exec.Cmd.Output", List((0, -1))),
    F("os/exec.Cmd.CombinedOutput", List((0, -1))),
    F("os/exec.Cmd.Run", List((0, -1))),
    F("os/exec.Cmd.Start", List((0, -1))),
    // === os package: file system operations ===
    F("os.Open", List((1, -1))),
    F("os.Create", List((1, -1))),
    F("os.OpenFile", List((1, -1))),
    F("os.ReadFile", List((1, -1))),
    F("os.WriteFile", List((1, 1), (2, 1))),
    F("os.Getenv", List((1, -1))),
    F("os.File.Read", List((0, 1), (0, -1))),
    F("os.File.Write", List((1, 0), (1, -1))),
    F("os.File.WriteString", List((1, 0), (1, -1))),
    // === io and io/ioutil: I/O operations ===
    F("io.ReadAll", List((1, -1))),
    F("io/ioutil.ReadAll", List((1, -1))),
    F("io/ioutil.ReadFile", List((1, -1))),
    F("io/ioutil.WriteFile", List((1, 1), (2, 1))),
    F("io.Copy", List((2, 1), (2, -1))),
    F("io.WriteString", List((2, 1), (2, -1))),
    // === html/template and text/template: XSS-relevant ===
    F("html/template.HTMLEscapeString", List((1, -1))),
    F("html/template.JSEscapeString", List((1, -1))),
    F("html/template.URLQueryEscaper", List((1, -1))),
    F("html/template.Template.Execute", List((1, 1), (2, 1))),
    F("html/template.Template.ExecuteTemplate", List((1, 1), (3, 1))),
    F("text/template.Template.Execute", List((1, 1), (2, 1))),
    // === encoding/json: serialization ===
    F("encoding/json.Marshal", List((1, -1))),
    F("encoding/json.Unmarshal", List((1, 2), (1, -1))),
    F("encoding/json.NewEncoder", List((1, -1))),
    F("encoding/json.NewDecoder", List((1, -1))),
    F("encoding/json.Encoder.Encode", List((1, 0), (1, -1))),
    F("encoding/json.Decoder.Decode", List((0, 1))),
    // === strings package: string operations ===
    PTF("strings.Join"),
    PTF("strings.Replace"),
    PTF("strings.ReplaceAll"),
    PTF("strings.ToLower"),
    PTF("strings.ToUpper"),
    PTF("strings.TrimSpace"),
    PTF("strings.Trim"),
    PTF("strings.TrimLeft"),
    PTF("strings.TrimRight"),
    PTF("strings.TrimPrefix"),
    PTF("strings.TrimSuffix"),
    PTF("strings.Split"),
    PTF("strings.SplitN"),
    F("strings.Contains", List((1, -1))),
    F("strings.NewReader", List((1, -1))),
    F("strings.Builder.WriteString", List((1, 0), (1, -1))),
    F("strings.Builder.String", List((0, -1))),
    // === strconv: type conversions ===
    F("strconv.Atoi", List((1, -1))),
    F("strconv.Itoa", List((1, -1))),
    PTF("strconv.FormatInt"),
    PTF("strconv.FormatFloat"),
    F("strconv.ParseInt", List((1, -1))),
    F("strconv.ParseFloat", List((1, -1))),
    // === path/filepath: path manipulation ===
    PTF("path/filepath.Join"),
    F("path/filepath.Base", List((1, -1))),
    F("path/filepath.Dir", List((1, -1))),
    F("path/filepath.Clean", List((1, -1))),
    F("path/filepath.Abs", List((1, -1))),
    // === regexp: regex operations ===
    F("regexp.Compile", List((1, -1))),
    F("regexp.MustCompile", List((1, -1))),
    F("regexp.Regexp.FindString", List((0, -1), (1, -1))),
    F("regexp.Regexp.FindStringSubmatch", List((0, -1), (1, -1))),
    F("regexp.Regexp.ReplaceAllString", List((0, -1), (1, -1), (2, -1))),
    // === bytes package ===
    F("bytes.NewBuffer", List((1, -1))),
    F("bytes.NewBufferString", List((1, -1))),
    F("bytes.Buffer.String", List((0, -1))),
    F("bytes.Buffer.Bytes", List((0, -1))),
    F("bytes.Buffer.Write", List((1, 0), (1, -1))),
    F("bytes.Buffer.WriteString", List((1, 0), (1, -1))),
    // === crypto/encoding: taint preservation ===
    F("crypto/sha256.Sum256", List((1, -1))),
    F("crypto/md5.Sum", List((1, -1))),
    F("encoding/base64.StdEncoding.EncodeToString", List((1, -1))),
    F("encoding/base64.StdEncoding.DecodeString", List((1, -1))),
    F("encoding/hex.EncodeToString", List((1, -1))),
    F("encoding/hex.DecodeString", List((1, -1)))
  )

  /** @return
    *   procedure semantics for operators and common external Java calls only.
    */
  @unused
  def javaSemantics(): FullNameSemantics = FullNameSemantics.fromList(operatorFlows ++ javaFlows)

}
