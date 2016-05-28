import Core.Conj

object Main {

  var depth = 0
  def spaces(n: Int): String = {
    n match {
      case 0 => ""
      case _ => "  " + spaces(n - 1)
    }
  }

  def prettyPrint(result: List[Conj]): String = {
    result.map(_.zipWithIndex.map{ case (e, i) => s"${i + 1}. $e" }.mkString("\n")).mkString("\n\n")
  }

  def collectRules(exprs: Conj): (Conj, List[Expr]) = {
    exprs.partition {
      case Implies(_, _) => false
      case _ => true
    }
  }

  def forward(rules: Seq[PExpr], state: PExpr, goal: PExpr): List[Conj] = {
    val (initialState, stateRules) = collectRules(state.toExpr)
    val (finalState, goalRules) = collectRules(goal.toExpr)
    forward(rules.flatMap(_.toExpr).toList ++ stateRules ++ goalRules, initialState, finalState, List[Expr]())
  }

  def forward(rules: Conj, state: Conj, goal: Conj, appliedRules: List[Expr]): List[Conj] = {

    println(spaces(depth) + "state " + state + " | applied " + appliedRules.reverse)
    depth = depth + 1

    var result = List[List[Expr]]() // TODO remove
    if (state.groupBy(identity) == goal.groupBy(identity)) {
      println("result! " + appliedRules.reverse)
      result = List(appliedRules.reverse)
    } else {
      val applicableRules = rules.filter(matchingRule(state, _))
      result = applicableRules match {
        case _ :: _ =>
          applicableRules.flatMap {
            case r @ Implies(left, right) =>
              val newState = right ++ normaliseAll(state diff left)
              forward(rules, newState, goal, r :: appliedRules)
            case r =>
              throw new RuntimeException(s"$r should not be in the list of rules")
          }
        case _ => List()
      }
    }
    depth = depth - 1
    result
  }

  def matchingRule(state: List[Expr], rule: Expr): Boolean = {
    rule match {
      case Implies(left, _) =>
        left.forall(state.contains(_))
      case _ =>
        throw new RuntimeException(s"rules should contain only implications, and not $rule")
    }
  }

  def normaliseAll(exprs: List[Expr]): List[Expr] = {
    val newExprs = exprs.map(normalise).filter(_ != Empty())
    if (newExprs != exprs) {
      normaliseAll(newExprs)
    } else {
      newExprs
    }
  }

  def normalise(expr: Expr): Expr = {
    expr match {
      case Empty() => expr
      case Term(v) => expr
      case Implies(l, r) => Implies(normaliseAll(l), normaliseAll(r))
    }
  }

  def main(args: Array[String]): Unit = {
    println("hi")
  }
}

