trait Expr

case class Sym(name: String) extends Expr
case class Cons(car: Expr, cdr: Expr) extends Expr
case class Num(value: Int) extends Expr
case class Str(value: String) extends Expr
case class Bool(value: Boolean) extends Expr
case class Nil() extends Expr
