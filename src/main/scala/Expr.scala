import Core.Conj

abstract sealed class Expr {

  def andToString(expr: Conj): String = {
    expr match {
      case e :: rest =>
        rest.foldLeft(e.toString) { (l, r) => s"$l & $r" }
      case _ => "<empty &>"
    }
  }

  override def toString: String = {
    this match {
      case Term(v) => v
      case Implies(l, r) => s"${andToString(l)} -o ${andToString(r)}"
      case Empty() => s"<empty>"
    }
  }
}

package object Core {
  type Conj = List[Expr]
}

case class Term(value: String) extends Expr
case class Implies(left: List[Expr], right: List[Expr]) extends Expr {
  override def equals(o: Any) = o match {
    case that: Implies =>
      that.left.groupBy(identity) == left.groupBy(identity) &&
        that.right.groupBy(identity) == right.groupBy(identity) ||
        that.left.groupBy(identity) == right.groupBy(identity) &&
          that.right.groupBy(identity) == left.groupBy(identity)
    case _ => false
  }
  override def hashCode = left.hashCode() + right.hashCode()
}
case class Empty() extends Expr

