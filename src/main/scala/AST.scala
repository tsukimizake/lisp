trait Expr {
  def show(): String = {
    this match {
      case Sym(name)      => name
      case Num(x)         => x.toString()
      case Str(x)         => "\"" + x + "\""
      case Bool(true)     => "#t"
      case Bool(false)    => "#f"
      case Nil()          => "Nil"
      case Cons(car, cdr) => "(" + car.show() + " " + cdr.show() + ")"
    }
  }
}

case class Sym(name: String) extends Expr
case class Cons(car: Expr, cdr: Expr) extends Expr
case class Num(value: Int) extends Expr
case class Str(value: String) extends Expr
case class Bool(value: Boolean) extends Expr
case class Nil() extends Expr
