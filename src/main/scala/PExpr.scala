import Core.Conj

abstract sealed class PExpr {

  def toExpr: List[Expr] = {
    preprocess(false)
  }

  def flatten(withinImpl: Boolean): List[Expr] = {
    this match {
      case PAnd(l, r) =>
        l.flatten(withinImpl) ++ r.flatten(withinImpl)
      case _ => preprocess(withinImpl)
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

  def preprocess(withinImpl: Boolean): Conj = {
    val result = reallyPreprocess(withinImpl)
    if (result.size == 1) {
      result.flatten
    } else {
      throw new RuntimeException(s"preprocessing $this resulted in $result")
    }
  }

  def reallyPreprocess(withinImpl: Boolean): List[Conj] = {
    this match {
      case PEmpty() => List(List(Empty()))
      case PTerm(v) => List(List(Term(v)))
      case PVar(v) => List(List(Var(v)))
      case PCompound(name, args) =>
        List(List(Compound(name, args.map { a =>
          val result = a.preprocess(withinImpl)
          if (result.size != 1) {
            throw new RuntimeException(s"invariant violated: $name has args $args")
          } else {
            result.head
          }
        })))
      case PAnd(l, r) => List(l.flatten(withinImpl) ++ r.flatten(withinImpl))
      case POr(l, r) =>
        if (withinImpl) {
          l.reallyPreprocess(true) ++ r.reallyPreprocess(true)
        } else {
          val term = nondeterministicTerm(l, r)
          val ll = l.reallyPreprocess(false)
          val rr = r.reallyPreprocess(false)
          List(term :: ll.map(Implies(List(term), _)) ++
            rr.map(Implies(List(term), _)) ++
            ll.map(Implies(_, List(term))) ++
            rr.map(Implies(_, List(term))))
        }
      case PImplies(l, r) =>
        List(l.reallyPreprocess(true).flatMap { ll =>
          r.reallyPreprocess(true).map { rr =>
            Implies(ll, rr)
          }
        })
    }
  }
}

case class PEmpty() extends PExpr
case class PTerm(value: String) extends PExpr
case class PVar(value: String) extends PExpr
case class PCompound(value: String, args: List[PExpr]) extends PExpr
case class PAnd(left: PExpr, right: PExpr) extends PExpr
case class POr(left: PExpr, right: PExpr) extends PExpr
case class PImplies(left: PExpr, right: PExpr) extends PExpr

