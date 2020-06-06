class EvalTest extends org.scalatest.FunSuite {

  test("(setq a '(hoge huga))") {
    assert(
      Parser.parseExpr("(setq a '(hoge huga))").map(_.eval()) == Right(Nil())
    )
  }

  test("sym") {
    assert(Parser.parseExpr("sym").map((_.eval())) == Right(Sym("sym")))
  }
}
