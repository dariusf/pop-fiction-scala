import fastparse.core.Parsed

object Main {

  var depth = 0
  def spaces(n: Int): String = {
    n match {
      case 0 => ""
      case _ => "  " + spaces(n - 1)
    }
  }

  def prettyPrint(result: List[List[Expr]]): String = {
    result.map(_.zipWithIndex.map{ case (e, i) => s"${i + 1}. $e" }.mkString("\n")).mkString("\n\n")
  }

  def collectRules(exprs: List[Expr]): (List[Expr], List[Expr]) = {
    exprs.partition {
      case Implies(_, _) => false
      case _ => true
    }
  }

  def forward(rules: Seq[PExpr], state: PExpr, goal: PExpr): List[List[Expr]] = {
    val (initialState, stateRules) = collectRules(state.toExpr.toList)
    val (finalState, goalRules) = collectRules(goal.toExpr.toList)
    forward(rules.map(_.toExpr).toList ++ stateRules ++ goalRules, initialState, finalState.head, List[Expr]())
  }

  def forward(rules: List[Expr], state: List[Expr], goal: Expr, appliedRules: List[Expr]): List[List[Expr]] = {

    println(spaces(depth) + "state " + state + " | applied " + appliedRules.reverse)
    depth = depth + 1

    var result = List[List[Expr]]() // TODO remove
    if (state == goal.toList) {
      println("result! " + appliedRules.reverse)
      result = List(appliedRules.reverse)
    } else {
      val applicableRules = rules.filter(matchingRule(state, _))
      result = applicableRules match {
        case _ :: _ =>
          applicableRules.flatMap {
            case r @ Implies(left, right) =>
              val newState = right :: normalise(remove(And(state), left)).toList
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

  def matchingRule(state: List[Expr], expr: Expr): Boolean = {
    expr match {
      case Empty() | Term(_) => state.contains(expr)
      case Implies(left, _) => matchingRule(state, left)
      case And(es) => es.forall(matchingRule(state, _))
    }
  }

  def remove(state: Expr, toRemove: Expr): Expr = {
    toRemove match {
      case Empty() => state
      case Term(_) | Implies(_, _) =>
        state match {
          case Empty() => state
          case Term(_) =>
            if (toRemove == state) {
              Empty()
            } else {
              state
            }
          case Implies(left, right) =>
            val newLeft = remove(left, toRemove)
            if (newLeft == left) {
              // Wasn't removed
              Implies(left, remove(right, toRemove))
            } else {
              Implies(newLeft, right)
            }
          case And(es) => And(removeFrom(es, toRemove))
        }
      case And(es) => es.foldRight(state)((t, c) => remove(c, t))
    }
  }

  def removeFrom(state: List[Expr], toRemove: Expr): List[Expr] = {
    state match {
      case x :: xs =>
        val attempt = remove(x, toRemove)
        if (attempt != x) {
          // Something was removed, we are done
          attempt :: xs
        } else {
          x :: removeFrom(xs, toRemove)
        }
      case _ =>
        throw new RuntimeException("should not happen, as an element must have matched")
    }
  }

  def main(args: Array[String]): Unit = {
    val Parsed.Success(expr, _) =
      Parser.parseExpr("(a | b)")
    println(expr.preprocess(false))
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
      case Implies(l, r) => Implies(normalise(l), normalise(r))
      case And(es) => And(normaliseAll(es))
    }
  }
}

