class ParserTest extends org.scalatest.FunSuite {
  test("symbol eq") {
    assert(Symbol("eu") == Symbol("eu"))

  }
  test("symbol list eq") {
    assert(List(Symbol("eu")) == List(Symbol("eu")))
  }
  test("symbol token") {
    // これが通らない
    // 左辺がToken型で右辺がSymbol型だと比較がうまくいかない
    assert(Parser.tokenize("honi")(0) == Symbol("honi"))

  }
  test("(a test)") {
    Parser.parseExpr("(a test)")
  }

  test("(a (nested) test)") {
    Parser.parseExpr("(a (nested) test)")
  }

  test("(a (dotted . list) test)") {
    Parser.parseExpr("(a (dotted . list) test)")
  }

  test("(a '(quoted (dotted . list)) test)") {
    Parser.parseExpr("(a '(quoted (dotted . list)) test)")
  }
}
