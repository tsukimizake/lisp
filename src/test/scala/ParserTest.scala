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
    assert(
      Parser.parseExpr("(a test)") ==
        Right(Cons(Sym("a"), Cons(Sym("test"), Nil())))
    )
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
    // format: off
    assert(
      Parser.parseExpr("(a (nested) test)")
        == 
          Right(
            Cons(
              Sym("a"),
              Cons(
                Cons(Sym("nested"), Nil()), 
                Cons(Sym("test"), Nil()))
            )
        )
  )
    // format: on
  }

  test("(a.dot)") {
    assert(
      Parser.parseExpr("(a.dot)")
        == Right(Cons(Sym("a"), Sym("dot")))
    )
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
    // format: off
    assert(
      Parser.parseExpr("(a (dotted . list) test)")
      == Right(
          Cons(
            Sym("a"),
            Cons(
              Cons(
                Sym("dotted"),
                Sym("list")
              ),
              Cons(
                Sym("test"),
                Nil()
              )
            )
          )
        )
    )
    // format: on
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
    //format: off
    assert(
      Parser.parseExpr("(a '(quoted (dotted . list)) test)")
        ==
          Right(
            Cons(
              Sym("a"),
              Cons(
                Cons(
                  Sym("quote"),
                  Cons(
                    Sym("quoted"),
                    Cons(
                      Cons(
                        Sym("dotted"),
                        Sym("list")
                      ),
                      Nil()
                    )
                  )
                ),
                Cons(
                  Sym("test"),
                  Nil()
                )
              )
            )
          )
    )
    //format: on
  }

  test("()") {
    assert(Parser.parseExpr("()") == Right(Nil()))
  }
  test("42") {
    assert(Tokenizer.tokenize("42") == List(NumLit(42)))
    assert(Parser.parseExpr("42") == Right(Num(42)))
  }

  test("\"hoge\"") {
    assert(Tokenizer.tokenize("\"hoge\"") == List(StrLit("hoge")))
    assert(Parser.parseExpr("\"hoge\"") == Right(Str("hoge")))
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

    // format: off
    assert(Parser.parseExpr("(a '(quoted (dotted . \"list\") 42) test)")
      == 
        Right(
          Cons(
            Sym("a"),
            Cons(
              Cons(
                Sym("quote"),
                Cons(
                  Sym("quoted"),
                  Cons(
                    Cons(
                      Sym("dotted"),
                      Str("list")
                    ),
                    Cons(
                      Num(42),
                      Nil()
                    )
                  )
                )
              ),
            Cons(
              Sym("test"),
              Nil()
            )
          )
        )
      )
    )
    // format: on
  }
}
