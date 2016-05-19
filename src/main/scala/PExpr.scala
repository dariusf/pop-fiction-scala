
abstract sealed class PExpr {
//  override def toString: String = {
//    this match {
//      case PTerm(v) => v
//      case PAnd(l, r) => s"($l & $r)"
//      case POr(l, r) => s"($l | $r)"
//      case PImplies(l, r) => s"$l -o $r"
//      case PEmpty() => s"empty"
//      //      case PPred(name, v) => s"$name(${v})"
//      //      case PVar(v) => v
//      //      case PUnbounded(v) => s"!$v"
//    }
//  }

  def toExpr: Expr = {
    val result = preprocess(false)
    if (result.length == 1) {
      result.head
    } else {
      And(result)
    }
  }

  def flatten(expr: PExpr, withinImpl: Boolean): List[Expr] = {
    expr match {
      case PAnd(l, r) =>
        flatten(l, withinImpl) ++ flatten(r, withinImpl)
      case _ => expr.preprocess(withinImpl)
    }
  }

  var freshness = 0
  def fresh(): Int = {
    freshness = freshness + 1
    freshness
  }

  def nondeterministicTerm(left: PExpr, right: PExpr): Expr = {
    // TODO the string form needs to be valid syntax so when it shows it doesn't look weird
    // Term(s"${left}_${right}_${fresh()}")
    Term(s"_${fresh()}")
  }

  def preprocess(withinImpl: Boolean): List[Expr] = {
    this match {
      case PEmpty() => List(Empty())
      case PTerm(v) => List(Term(v))
      case PAnd(l, r) => List(And(flatten(l, withinImpl) ++ flatten(r, withinImpl)))
      case POr(l, r) =>
        if (withinImpl) {
          l.preprocess( true) ++ r.preprocess(true)
        } else {
          val term = nondeterministicTerm(l, r)
          term :: l.preprocess(false).map(Implies(term, _)) ++
            r.preprocess(false).map(Implies(term, _)) ++
            l.preprocess(false).map(Implies(_, term)) ++
            r.preprocess(false).map(Implies(_, term))
        }
      case PImplies(l, r) =>
        l.preprocess(true).flatMap { ll =>
          r.preprocess(true).map { rr =>
            Implies(ll, rr)
          }
        }
    }
  }
}

case class PEmpty() extends PExpr
case class PTerm(value: String) extends PExpr
case class PAnd(left: PExpr, right: PExpr) extends PExpr {
  override def equals(o: Any) = o match {
    case that: PAnd =>
      that.left == left && that.right == right || that.left == right && that.right == left
    case _ => false
  }
  override def hashCode = left.hashCode() + right.hashCode()
}
case class POr(left: PExpr, right: PExpr) extends PExpr {
  override def equals(o: Any) = o match {
    case that: POr =>
      that.left == left && that.right == right || that.left == right && that.right == left
    case _ => false
  }
  override def hashCode = left.hashCode() + right.hashCode()
}
case class PImplies(left: PExpr, right: PExpr) extends PExpr

//case class PPred(name: String, expr: PExpr) extends PExpr
//case class PVar(value: String) extends PExpr

//case class PUnbounded(expr: PExpr) extends PExpr


