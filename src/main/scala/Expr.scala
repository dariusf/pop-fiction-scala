
abstract sealed class Expr {
  override def toString: String = {
    this match {
      case Term(v) => v
      //      case Pred(name, v) => s"$name(${v})"
      //      case Var(v) => v
      //      case And(l, r) => s"$l * $r"
      //      case Or(l, r) => s"$l & $r"
      case Implies(l, r) => s"$l -o $r"
      case And(es) =>
        es match {
          case e :: rest =>
            rest.foldLeft(e.toString){ (l, r) => s"$l & $r" }
          case _ => "<empty &>"
        }
      case Empty() => s"<empty>"
      //      case Unbounded(v) => s"!$v"
    }
  }

  def toList: List[Expr] = {
    this match {
      case Empty() => List()
      case Term(v) => List(this)
      case Implies(l, r) => List(this)
      case And(es) => es
    }
  }
}

case class Term(value: String) extends Expr
case class And(exprs: List[Expr]) extends Expr
case class Implies(left: Expr, right: Expr) extends Expr
case class Empty() extends Expr

//case class Pred(name: String, expr: Expr) extends Expr
//case class Var(value: String) extends Expr

//case class Or(left: Expr, right: Expr) extends Expr

//case class Unbounded(expr: Expr) extends Expr


