import Core.Conj

object Main {

  var mainDepth = 0
  var auxDepth = 0
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

  def runToGoal(rules: Seq[PExpr], state: PExpr, goal: PExpr): List[List[Expr]] = {
    // We need this because OR in state/goals will be translated into more rules
    val (initialState, stateRules) = collectRules(state.toExpr)
    val (finalState, goalRules) = collectRules(goal.toExpr)
    val allRules = rules.flatMap(_.toExpr).toList ++ stateRules ++ goalRules

    run(allRules, initialState, finalState, { (state, goal) =>
      state.groupBy(identity) == goal.groupBy(identity)
    }, List()).distinct
  }

  def run(rules: Conj,
          state: Conj,
          goal: Conj,
          endCondition: (Conj, Conj) => Boolean,
          appliedRules: List[Expr]
         ): List[List[Expr]] = {

//    println(s"${spaces(mainDepth)}forward state $state | applied ${appliedRules.reverse}")
    mainDepth = mainDepth + 1

    var result = List[List[Expr]]() // TODO remove
    if (endCondition(state, goal)) {
      println(s"result! ${appliedRules.reverse}")
      result = List(appliedRules.reverse)
    } else {
      val applicableRules = rules.filter(matchingRule(state, _))
//      println(s"${spaces(mainDepth)}applicable rules $applicableRules")
      result = applicableRules match {
        case _ :: _ =>
          applicableRules.flatMap {
            case r @ Implies(left, right) =>
              if (freeVars(r).isEmpty) {
                val newState = right ++ state diff left
                run(rules, newState, goal, endCondition, r :: appliedRules)
              } else {
                // TODO stdlib method
                val possibleRuleOrderings = permutations(left)
                possibleRuleOrderings.flatMap { ordering =>
                  attemptToUnify(ordering, state).flatMap { subs =>
                    if (subs.isEmpty) {
                      List()
                    } else {
                      val additions = right.map(apply(subs, _))
                      val subtractions = left.map(apply(subs, _))
                      val newState = additions ++ state diff subtractions
                      run(rules, newState, goal, endCondition, apply(subs, r) :: appliedRules)
                    }
                  }
                }
              }
            case r =>
              throw new RuntimeException(s"$r should not be in the list of rules")
          }
        case _ => List()
      }
    }
    mainDepth = mainDepth - 1
    result
  }

  def attemptToUnify(ruleLHS: Conj, state: Conj): List[List[Sub]] = {
//    println(s"${spaces(mainDepth)} applying rule with lhs $ruleLHS")
//    println(s"${spaces(mainDepth)} to state $state")
    def aux(ruleLHS: List[Expr], state: Conj, soFar: List[Sub]): List[List[Sub]] = {
      auxDepth = auxDepth + 1
//      println(s"${spaces(mainDepth + auxDepth)}aux $ruleLHS | $soFar")
      val result =
        ruleLHS match {
        case x :: xs =>
            val subss = everyItem(state).flatMap{
              case (s, newState) =>
                try {
                 List((unify(x, s), newState))
                } catch {
                  case e: UnificationException =>
//                    println(spaces(mainDepth + auxDepth) + e.getMessage)
                    List()
                }
            }
//            println(s"${spaces(mainDepth + auxDepth)}subss $subss")
            val result =
              subss.flatMap { case (subs, newState) =>
                val newLHS = xs.map(apply(subs ++ soFar, _))
//                println(s"${spaces(mainDepth + auxDepth)}continuing down the lhs $newLHS")
                aux(newLHS, newState, subs ++ soFar)
              }
//            println(s"${spaces(mainDepth + auxDepth)}result $result")
            result
        case _ => List(soFar)
      }
      auxDepth = auxDepth - 1
      result
    }
    val result = aux(ruleLHS, state, List())
//    println(s"${spaces(mainDepth)}final aux result $result")
    result
  }

  def main(args: Array[String]): Unit = {
    println(attemptToUnify(
      List(Compound("at", List(Var("A"), Var("B"))), Compound("at", List(Var("B"), Var("C")))),
      List(Compound("at", List(Term("a"), Term("b"))), Compound("at", List(Term("b"), Term("c"))))
    ))
  }

  def everyItem[A](things: List[A]): List[(A, List[A])] = {
    def aux[A](things: List[A], collected: List[A]): List[(A, List[A])] = {
      things match {
        case x :: xs =>
          (x, collected ++ xs) :: aux(xs, x :: collected)
        case _ =>
          List()
      }
    }
    aux(things, List())
  }

  def permutations[A](things: List[A]): List[List[A]] = {
    things match {
      case _ :: _ =>
        everyItem(things).flatMap { case (x, xs) => permutations(xs).map(x :: _) }
      case _ => List(List())
    }
  }

  def matchingRule(state: List[Expr], rule: Expr): Boolean = {
    rule match {
      case Implies(left, _) =>
        left.forall(l => state.contains(l) || state.exists(s =>
          try {
            unify(s, l).nonEmpty
          } catch {
            case e: UnificationException => false
          }))
      case _ =>
        throw new RuntimeException(s"rules should contain only implications, and not $rule")
    }
  }

  case class Sub(variable: String, term: Expr) {
    override def toString: String = {
      s"$variable -> $term"
    }
  }

  class UnificationException(message: String = null) extends RuntimeException(message)

  def unify(left: Expr, right: Expr): List[Sub] = {
    (left, right) match {
      case (Term(x), Term(y)) =>
        if (x == y) {
          List()
        } else {
          throw new UnificationException(s"could not unify $left and $right")
        }
      case (Empty(), Empty()) => List()
      case (Implies(l1, r1), Implies(l2, r2)) =>
        l1.zip(l2).flatMap { case (x, y) => unify(x, y) } ++
          r1.zip(r2).flatMap { case (x, y) => unify(x, y) }
      case (Compound(n1, args1), Compound(n2, args2)) =>
        if (n1 == n2) {
          args1.zip(args2).flatMap { case (x, y) => unify(x, y) }
        } else {
          throw new UnificationException(s"could not unify $left and $right")
        }
      case (Var(x), _) =>
        if (left == right || !occursIn(left, right)) {
          List(Sub(x, right))
        } else {
          throw new UnificationException(s"could not unify $left and $right")
        }
      case (_, Var(_)) => unify(right, left)
      case _ =>
        throw new UnificationException(s"could not unify $left and $right")
    }
  }

  def freeVars(expr: Expr): List[String] = {
    expr match {
      case Term(_) | Empty() => List()
      case Var(v) => List(v)
      case Implies(l, r) =>
        l.flatMap(freeVars(_)) ++
          r.flatMap(freeVars(_))
      case Compound(_, args) =>
        args.flatMap(freeVars(_))
    }
  }

  def occursIn(needle: Expr, haystack: Expr): Boolean = {
    needle match {
      case Term(_) | Empty() => false
      case Var(_) => haystack == needle
      case Implies(l, r) =>
        l.map(occursIn(needle, _)).forall(identity) &&
          r.map(occursIn(needle, _)).forall(identity)
      case Compound(_, args) =>
        args.map(occursIn(needle, _)).forall(identity)
    }
  }

  def apply(subs: List[Sub], term: Expr): Expr = {
    subs.foldRight(term)((s, e) => apply(s, e))
  }

  def apply(sub: Sub, term: Expr): Expr = {
    val Sub(k, t) = sub
    term match {
      case Empty() | Term(_) => term
      case Var(v) =>
        if (k == v) t else term
      case Implies(l, r) =>
        Implies(l.map(apply(sub, _)), r.map(apply(sub, _)))
      case Compound(name, args) =>
        Compound(name, args.map(apply(sub, _)))
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
      case Var(v) => expr
      case Compound(name, args) => Compound(name, normaliseAll(args))
      case Implies(l, r) => Implies(normaliseAll(l), normaliseAll(r))
    }
  }
}

