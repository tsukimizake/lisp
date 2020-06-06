import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer

trait SemanticError
case class ListCastError(err: String) extends SemanticError
case class NumCastError(err: String) extends SemanticError

object Eval {
  private var env: ListBuffer[Map[String, Expr]] =
    new ListBuffer[Map[String, Expr]]()
  def eval(x: Expr): Expr = {
    x match {
      case x: Nil  => Nil()
      case x: Sym  => x // TODO
      case x: Num  => x
      case x: Str  => x
      case x: Bool => x
      case x: Cons => {
        x.car match {
          case Sym("lambda") => {
            pushEnv()
            val name = genSym("lambda")
            ???
          }
          case Sym("quote") => x.cdr
          case Sym("setq") => {
            x.cdr match {
              case cd: Cons => {
                cd.car match {
                  case Sym(name) => { env = defName(name, cd.cdr); Nil() }
                }
              }
            }
          }
          case Sym(name) => {
            findName(name)
          }
        }
      }

    }
  }
  private val mathPrims: List[(String, Expr => Either[SemanticError, Num])] =
    List(
      ("+", mathOp((a: Int, b: Int) => a + b)),
      ("-", mathOp((a: Int, b: Int) => a - b)),
      ("*", mathOp((a: Int, b: Int) => a * b)),
      ("/", mathOp((a: Int, b: Int) => a / b)),
      ("mod", mathOp((a: Int, b: Int) => a % b))
    )
  private def asList(expr: Expr): Either[ListCastError, List[Expr]] = {
    expr match {
      case Cons(car, cdr) =>
        return for (rest <- asList(cdr))
          yield (car :: rest)
      case _ => Left(new ListCastError(expr.toString()))
    }
  }
  private def mathOp(
      f: (Int, Int) => Int
  )(expr: Expr): Either[SemanticError, Num] = {
    def fNum(l: Expr, r: Expr): Either[SemanticError, Num] = {
      l match {
        case Num(lv) =>
          r match {
            case Num(rv) =>
              Right(Num(f(lv, rv)))
            case otherwise => Left(new NumCastError(r.toString()))
          }
        case otherwise => Left(new NumCastError(l.toString()))
      }
    }

    for (list <- asList(expr);
         res <- list.map(_.eval()).reduce(fNum))
      yield (
        res
      )
    ???
  }
  private def apply(f: Expr, xs: Expr): Expr = ???
  private def findName(s: String) = ???
  private def pushEnv() = ???
  private def popEnv() = ???
  private def defName(name: String, body: Expr) = ???
  private def genSym(prefix: String): String = ???
}
