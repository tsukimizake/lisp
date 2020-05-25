class ParserTest extends org.scalatest.FunSuite {
  test("symbol token") {
    assert(Tokenizer.tokenize("honi") == List(Symbol("honi")))
  }
  test("(a test)") {
    assert(
      Tokenizer.tokenize("(a test)") == List(
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
      Tokenizer.tokenize("(a (nested) test)") == List(
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
    assert(
      Tokenizer.tokenize("(a (dotted . list) test)") == List(
        OParen,
        Symbol("a"),
        OParen,
        Symbol("dotted"),
        Dot,
        Symbol("list"),
        CParen,
        Symbol("test"),
        CParen
      )
    )
    Parser.parseExpr("(a (dotted . list) test)")
  }

  test("(a '(quoted (dotted . list)  ) test)") {
    assert(
      Tokenizer.tokenize("(a '(quoted (dotted . list)) test)") == List(
        OParen,
        Symbol("a"),
        Quote,
        OParen,
        Symbol("quoted"),
        OParen,
        Symbol("dotted"),
        Dot,
        Symbol("list"),
        CParen,
        CParen,
        Symbol("test"),
        CParen
      )
    )
    Parser.parseExpr("(a '(quoted (dotted . list)) test)")
  }

  test("42") {
    assert(Tokenizer.tokenize("42") == List(NumLit(42)))
  }

  test("\"hoge\"") {
    assert(Tokenizer.tokenize("\"hoge\"") == List(StrLit("hoge")))
  }

  test("(a '(quoted (dotted . \"list\") 42) test)") {
    assert(
      Tokenizer.tokenize("(a '(quoted (dotted . \"list\") 42) test)") == List(
        OParen,
        Symbol("a"),
        Quote,
        OParen,
        Symbol("quoted"),
        OParen,
        Symbol("dotted"),
        Dot,
        StrLit("list"),
        CParen,
        NumLit(42),
        CParen,
        Symbol("test"),
        CParen
      )
    )
    Parser.parseExpr("(a '(quoted (dotted . list)) test)")
  }
}
