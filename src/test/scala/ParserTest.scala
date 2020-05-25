class ParserTest extends org.scalatest.FunSuite {
  test("symbol token") {
    assert(Parser.tokenize("honi")(0) == new Symbol("honi"))
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
