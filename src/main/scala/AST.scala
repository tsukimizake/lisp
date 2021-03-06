trait Expr {
  override def toString(): String = {
    this match {
      case Sym(name)   => name
      case Num(x)      => x.toString()
      case Str(x)      => "\"" + x + "\""
      case Bool(true)  => "#t"
      case Bool(false) => "#f"
      case Nil()       => "Nil"
      // TODO: listを(a (b (c Nil)))でなく(a b c)で出したい
      case Cons(car, cdr) =>
        "(" + car.toString() + " " + cdr.toString() + ")"
    }
  }

  def eval() : Expr = {
    Eval.eval(this)
  }
}

case class Sym(name: String) extends Expr
case class Cons(car: Expr, cdr: Expr) extends Expr
case class Num(value: Int) extends Expr
case class Str(value: String) extends Expr
case class Bool(value: Boolean) extends Expr
case class Nil() extends Expr
