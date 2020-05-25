class ParserTest extends org.scalatest.FunSuite {
  test("symbol token") {
    assert(Parser.tokenize("honi") == List(Symbol("honi")))
  }
  test("(a test)") {
    assert(
      Parser.tokenize("(a test)") == List(
        OParen,
        Symbol("a"),
        Symbol("test"),
        CParen
      )
    )
    Parser.parseExpr("(a test)")
  }

  test("(a (nested ) test)") {
    assert(
      Parser.tokenize("(a (nested) test)") == List(
        OParen,
        Symbol("a"),
        OParen,
        Symbol("nested"),
        CParen,
        Symbol("test"),
        CParen
      )
    )
    Parser.parseExpr("(a (nested) test)")
  }

  test("(a (dotted . list) test)") {
    Parser.parseExpr("(a (dotted . list) test)")
  }

  test("(a '(quoted (dotted . list)) test)") {
    Parser.parseExpr("(a '(quoted (dotted . list)) test)")
  }
}
