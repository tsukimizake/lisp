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

object QuoteSym extends Symbol("quote")

object Parser {
  def parseExpr(input: String) = {
    val tokens = Tokenizer.tokenize(input)
    sexp(tokens)

  }

  private def sexp(tokens: List[Token]): Expr = {
    tokens(0) match {
      case OParen    => list(tokens)
      case x: Symbol => x.castExpr()
      case x: NumLit => x.castExpr()
      case x: StrLit => x.castExpr()
      case Quote => {
        val tail = tokens.drop(1)
        tail.head match {
          case x: Symbol =>
            sexp(
              List(OParen, QuoteSym, tail.head, CParen) ++ tail.drop(1)
            )
          case OParen =>
            sexp(
              List(OParen, QuoteSym) ++ tail.drop(1)
            )
          case _ => ???
        }
      }
      case NilTok => Nil()
      case CParen => Nil()
      case Dot => {
        val tail = tokens.drop(1)
        sexp(List(tail.head))
      }
    }
  }

  private def list(tokens: List[Token]): Expr = {
    val tail = tokens.drop(1)
    val car = sexp(tail)
    Cons(sexp(List(tail(0))), sexp(tail.drop(1).dropRight(1))) //うそ

  }

  def parseStatement(input: String): List[Expr] = ???
  def parseFile(file: File): Expr = ???
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
