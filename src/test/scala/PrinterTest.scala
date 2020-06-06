class PrinterTest extends org.scalatest.FunSuite {

  test("(a test)") {
    assert(
      Parser.parseExpr("(a test)").map(_.toString()) == Right(
        "(a (test Nil))"
      )
    )
  }

}
