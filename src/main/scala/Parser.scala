import java.io.File
import scala.collection.mutable
import util.control.Breaks._

sealed trait Token

object OParen extends Token
object CParen extends Token
object Apostroph extends Token
object NilTok extends Token
sealed class StrLit(str: String) extends Token
sealed class NumLit(value: Int) extends Token
sealed class Symbol(name: String) extends Token {
  //def name(): String = name
  //override def equals(x: Token): Boolean = {
  //  x match {
  //    case x: Symbol => this.name == x.name()
  //    case _         => false
  //  }
  //}
}

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
              new StrLit(str)
            } else if (input.substring(iter, iter + 2) == "Nil") {
              iter += 3; NilTok
            }
//          else if ('0' <= input.charAt(iter) && input.charAt(iter) <= '9'){}
            else { // symbol
              var i = iter
              var str = ""
              while (i != input.length() && input.charAt(i) != ' ') {
                str += input.charAt(i)
                i += 1
              }
              iter = i
              println(str)
              new Symbol(str)
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
