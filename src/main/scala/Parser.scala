import java.io.File
import scala.collection.mutable
import util.control.Breaks._

sealed trait Token

case object OParen extends Token
case object CParen extends Token
case object Apostroph extends Token
case object NilTok extends Token
case class StrLit(str: String) extends Token
case class NumLit(value: Int) extends Token
case class Symbol(name: String) extends Token

object Parser {
  def parseExpr(input: String) = {}

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
            if (input.charAt(iter) == '(') {
              iter += 1; OParen
            } else if (input.charAt(iter) == ')') { iter += 1; CParen }
            else if (input.charAt(iter) == '\'') { iter += 1; Apostroph }
            else if (input.charAt(iter) == '"') {
              var i = iter + 1
              var str = ""
              while (i != input.length() &&
                     !(input.charAt(i) == '"' && input.charAt(i - 1) != '\\')) {
                str += input.charAt(i)
                i += 1
              }
              iter = i
              StrLit(str)
            } else if (input.substring(iter, iter + 2) == "Nil") {
              iter += 3; NilTok
            } else if ('0' <= input.charAt(iter) && input.charAt(iter) <= '9') {
              var buf = ""
              var i = iter
              while (i != input.length()) {
                buf += input.charAt(i)
                i += 1
              }
              NumLit(buf.toInt)
            } else { // symbol
              var i = iter
              var str = ""
              while (i != input.length() && input.charAt(i) != ' ') {
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

  def parseStatement(input: String): List[Expr] = ???
  def parseFile(file: File): Expr = ???
}
