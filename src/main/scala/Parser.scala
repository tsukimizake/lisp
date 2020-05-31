import java.io.File
import scala.collection.mutable
import util.control.Breaks._
import scala.language.implicitConversions

sealed trait Token

case object OParen extends Token
case object CParen extends Token
case object Quote extends Token
case object Dot extends Token
case object NilTok extends Token
case class StrLit(str: String) extends Token {
  def castExpr(): Expr = { Str(this.str) }
}
case class NumLit(value: Int) extends Token {
  def castExpr(): Expr = { Num(this.value) }
}
case class Symbol(name: String) extends Token { // TODO 変形シングルトンにしてeqをアドレス比較
  implicit def castExpr(): Expr = { Sym(this.name) }
}

case class ParseError(message: String) {}

object QuoteSym extends Sym("quote")

sealed class SexpParser(tokens: List[Token], var iter: Int) {

  def sexp(): Either[ParseError, Expr] = {
    //println("sexp " + iter + tokens(iter))
    tokens(iter) match {
      case OParen    => { moveIter(1); list() }
      case x: Symbol => { moveIter(1); Right(x.castExpr()) }
      case x: NumLit => { moveIter(1); Right(x.castExpr()) }
      case x: StrLit => { moveIter(1); Right(x.castExpr()) }
      case Quote => {
        moveIter(1);
        for (cdr <- sexp())
          yield (Cons(QuoteSym, cdr))
      }
      case NilTok => { moveIter(1); Right(Nil()) }
      case CParen => Left(ParseError("unexpected ) as sexp"))
      case Dot    => Left(ParseError("unexpected . as sexp"))
    }
  }

  private[this] def moveIter(n: Int) {
    iter += n
  }

  private[this] def list(): Either[ParseError, Expr] = {
    //println("list " + iter + tokens(iter))
    if (tokens(iter) == CParen) { moveIter(1); return Right(Nil()) }
    if (tokens(iter) == Dot) {
      if (tokens(iter + 2) != CParen) {
        Left(ParseError("unexpected . in list"))
      }
      moveIter(1)
      val cdr = sexp()
      moveIter(1)
      return cdr
    }
    for (car <- sexp();
         cdr <- list()) yield (Cons(car, cdr))
  }
}

object Parser {
  def parseExpr(input: String) : Either[ParseError, Expr]={
    val tokens = Tokenizer.tokenize(input)
    new SexpParser(tokens, 0).sexp()

  }

  def parseStatement(input: String): Either[ParseError, List[Expr]] = ???
  def parseFile(file: File): Either[ParseError, Expr] = ???
}

object Tokenizer {
  def tokenize(input: String): List[Token] = {
    var iter: Int = 0;
    var buf: mutable.ListBuffer[Token] = new mutable.ListBuffer()

    while (iter < input.length()) {
      breakable {
        if (input.charAt(iter) == ' '
            || input.charAt(iter) == '\t'
            || input.charAt(iter) == '\n') {
          iter += 1
          break() //continue
        } else {
          val tok: Token =
            if (input.charAt(iter) == '(') { iter += 1; OParen }
            else if (input.charAt(iter) == ')') {
              iter += 1; CParen
            } else if (input.charAt(iter) == '\'') { iter += 1; Quote }
            else if (input.charAt(iter) == '"') {
              var i = iter + 1
              var str = ""
              while (i != input.length() &&
                     !(input.charAt(i) == '"' && input.charAt(i - 1) != '\\')) { // TODO "\\"の扱い?
                str += input.charAt(i)
                i += 1
              }
              iter = i + 1
              StrLit(str)
            } else if (input.charAt(iter) == '.') {
              iter += 1; Dot
            } else if (input.substring(iter, iter + 2) == "Nil") {
              iter += 3; NilTok
            } else if (('0' to '9').contains(input.charAt(iter))) {
              var buf = ""
              var i = iter
              while (i != input.length()
                     && (('0' to '9').contains(input.charAt(i)))) {
                buf += input.charAt(i)
                i += 1
              }
              iter = i
              NumLit(buf.toInt)
            } else { // symbol
              var i = iter
              var str = ""
              while (i != input.length()
                     && !"()\".' \t\n".contains(input.charAt(i))) {
                str += input.charAt(i)
                i += 1
              }
              iter = i
              Symbol(str)
            }
          buf.addOne(tok)
        }
      }
    }
    return buf.toList
  }
}
