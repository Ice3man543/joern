package io.joern.gosrc2cpg.astcreation

/** Hardcoded type mappings for common Go standard library struct fields and method return types.
  *
  * When gosrc2cpg encounters a field access like `r.URL` where `r` is `*net/http.Request`,
  * it needs to know that `.URL` has type `*net/url.URL`. Without analyzing the stdlib source,
  * this information is unavailable and the type falls back to `<FieldAccess>.<unknown>`,
  * which breaks method resolution for chained calls like `r.URL.Query().Get("key")`.
  *
  * This map provides the missing type information for the most commonly used stdlib types.
  */
object GoStdlibTypeMap {

  /** Maps "parentType.fieldName" -> "fieldType" for stdlib struct fields. */
  val structMemberTypes: Map[String, String] = Map(
    // net/http.Request fields
    "net/http.Request.URL"            -> "*net/url.URL",
    "net/http.Request.Header"         -> "net/http.Header",
    "net/http.Request.Body"           -> "io.ReadCloser",
    "net/http.Request.GetBody"        -> "func() (io.ReadCloser, error)",
    "net/http.Request.Form"           -> "net/url.Values",
    "net/http.Request.PostForm"       -> "net/url.Values",
    "net/http.Request.MultipartForm"  -> "*mime/multipart.Form",
    "net/http.Request.Method"         -> "string",
    "net/http.Request.Proto"          -> "string",
    "net/http.Request.Host"           -> "string",
    "net/http.Request.RemoteAddr"     -> "string",
    "net/http.Request.RequestURI"     -> "string",
    "net/http.Request.TLS"            -> "*crypto/tls.ConnectionState",
    "net/http.Request.Response"       -> "*net/http.Response",
    // net/http.Response fields
    "net/http.Response.Body"          -> "io.ReadCloser",
    "net/http.Response.Header"        -> "net/http.Header",
    "net/http.Response.StatusCode"    -> "int",
    "net/http.Response.Status"        -> "string",
    "net/http.Response.Request"       -> "*net/http.Request",
    // net/url.URL fields
    "net/url.URL.Scheme"              -> "string",
    "net/url.URL.Opaque"              -> "string",
    "net/url.URL.User"                -> "*net/url.Userinfo",
    "net/url.URL.Host"                -> "string",
    "net/url.URL.Path"                -> "string",
    "net/url.URL.RawPath"             -> "string",
    "net/url.URL.RawQuery"            -> "string",
    "net/url.URL.Fragment"            -> "string",
    // os/exec.Cmd fields
    "os/exec.Cmd.Path"               -> "string",
    "os/exec.Cmd.Args"               -> "[]string",
    "os/exec.Cmd.Dir"                -> "string",
    "os/exec.Cmd.Env"                -> "[]string",
    "os/exec.Cmd.Stdin"              -> "io.Reader",
    "os/exec.Cmd.Stdout"             -> "io.Writer",
    "os/exec.Cmd.Stderr"             -> "io.Writer",
    // database/sql types
    "database/sql.DB.Stats"           -> "database/sql.DBStats",
    "database/sql.Tx.Conn"            -> "database/sql.Conn",
    // net/http.Header (alias for map[string][]string) — treat as its own type
    // net/url.Values (alias for map[string][]string) — treat as its own type
    // os.File fields
    "os.File.Name"                    -> "string",
    // http.Cookie fields
    "net/http.Cookie.Name"            -> "string",
    "net/http.Cookie.Value"           -> "string",
    "net/http.Cookie.Path"            -> "string",
    "net/http.Cookie.Domain"          -> "string"
  )

  /** Maps "receiverType.methodName" -> "returnType" for stdlib methods. */
  val methodReturnTypes: Map[String, String] = Map(
    // net/http.Request methods
    "net/http.Request.FormValue"      -> "string",
    "net/http.Request.PostFormValue"   -> "string",
    "net/http.Request.Cookie"         -> "*net/http.Cookie",
    "net/http.Request.Cookies"        -> "[]*net/http.Cookie",
    "net/http.Request.Referer"        -> "string",
    "net/http.Request.UserAgent"      -> "string",
    "net/http.Request.Context"        -> "context.Context",
    // net/url.URL methods
    "net/url.URL.Query"               -> "net/url.Values",
    "net/url.URL.String"              -> "string",
    "net/url.URL.Hostname"            -> "string",
    "net/url.URL.Port"                -> "string",
    "net/url.URL.EscapedPath"         -> "string",
    "net/url.URL.EscapedFragment"     -> "string",
    // net/url.Values methods
    "net/url.Values.Get"              -> "string",
    "net/url.Values.Encode"           -> "string",
    // net/http.Header methods
    "net/http.Header.Get"             -> "string",
    "net/http.Header.Values"          -> "[]string",
    // os/exec.Cmd methods
    "os/exec.Cmd.Output"              -> "[]byte",
    "os/exec.Cmd.CombinedOutput"      -> "[]byte",
    // database/sql methods
    "database/sql.DB.Query"           -> "*database/sql.Rows",
    "database/sql.DB.QueryRow"        -> "*database/sql.Row",
    "database/sql.DB.Exec"            -> "database/sql.Result",
    "database/sql.DB.Prepare"         -> "*database/sql.Stmt",
    "database/sql.DB.Begin"           -> "*database/sql.Tx",
    "database/sql.Tx.Query"           -> "*database/sql.Rows",
    "database/sql.Tx.QueryRow"        -> "*database/sql.Row",
    "database/sql.Tx.Exec"            -> "database/sql.Result",
    "database/sql.Tx.Prepare"         -> "*database/sql.Stmt",
    // io
    "io.ReadCloser.Read"              -> "int",
    // os
    "os.File.Read"                    -> "int",
    "os.File.Write"                   -> "int"
  )

  /** Resolve a struct field type from the stdlib map.
    * @param receiverType e.g. "net/http.Request" (pointer prefix already stripped)
    * @param fieldName e.g. "URL"
    * @return Some("*net/url.URL") or None
    */
  def resolveStructMember(receiverType: String, fieldName: String): Option[String] = {
    val key = s"${receiverType.stripPrefix("*")}.$fieldName"
    structMemberTypes.get(key)
  }

  /** Resolve a method return type from the stdlib map.
    * @param receiverType e.g. "net/url.URL"
    * @param methodName e.g. "Query"
    * @return Some("net/url.Values") or None
    */
  def resolveMethodReturn(receiverType: String, methodName: String): Option[String] = {
    val key = s"${receiverType.stripPrefix("*")}.$methodName"
    methodReturnTypes.get(key)
  }
}
