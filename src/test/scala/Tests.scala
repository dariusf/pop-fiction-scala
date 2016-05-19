
import fastparse.core.Parsed
import org.scalatest._

class Tests extends FlatSpec with Matchers {

  "remove" should "work correctly" in {
    def test(expr: Expr, toRemove: Expr): Expr =
      Main.normalise(Main.remove(expr, toRemove))

    test(Term("a"), Term("a")) should be (Empty())
    test(And(List(Term("b"), Term("a"))), Term("a")) should be (And(List(Term("b"))))
    test(And(List(Term("b"), Term("a"))), Term("b")) should be (And(List(Term("a"))))
  }

  "forward" should "work with terms" in {
    test(
      """
        |b -o pig.
        |pig -o house.
      """.stripMargin,
      "b",
      "house"
    ) should be (List(rules(
      """
        |b -o pig.
        |pig -o house.
      """.stripMargin)))
  }

  "forward" should "work with nondeterministic state/endings" in {
    test(
      """
        |a -o b.
      """.stripMargin,
      "a",
      "b | c"
    ) should be (List(rules("a -o b.") ++ List(Implies(Term("b"), Term("_1")))))
  }

  "forward" should "work with &" in {
    test(
      """
        |a & b -o house.
      """.stripMargin,
      "a & b",
      "house"
    ) should be (List(rules(
      """
        |a & b -o house.
      """.stripMargin)))

    test(
      """
        |a & b -o house.
      """.stripMargin,
      "b & a",
      "house"
    ) should be (List(rules(
      """
        |a & b -o house.
      """.stripMargin)))
  }

//  "forward" should "work with rules with | on the left side" in {
//    val rules = List(PImplies(POr(pig, b), house))
//    Main.forward(rules, b, house, List()) should
//      be (List(rules))
//    Main.forward(rules, pig, house) should
//      be (List(rules))
//  }
//
//  "forward" should "work with rules with & on the left side" in {
//    val rules = List(PImplies(PAnd(pig, b), house))
//    Main.forward(rules, PAnd(pig, b), house, List()) should
//      be (List(rules))
//    Main.forward(rules, PAnd(b, pig), house) should
//      be (List(rules))
//  }
//
//  "forward" should "work with rules with & in the state" in {
//    val rules = List(PImplies(pig, house))
//    Main.forward(rules, PAnd(b, pig), PAnd(b, house), List()) should
//      be (List(rules))
//  }
//
//  "forward" should "backtrack correctly" in {
//    val rules = List(PImplies(pig, b), PImplies(PAnd(pig, house), b))
//    Main.forward(rules, PAnd(pig, house), b, List()) should
//      be (List(List(PImplies(PAnd(pig, house), b))))
//  }
//

  "three little pigs" should "work!" in {
    val r = """
        |pig & straw -o straw_house.
        |pig & sticks -o stick_house.
        |pig & bricks -o brick_house.
        |straw_house & wolf -o wolf.
        |stick_house & wolf -o wolf.
        |brick_house & wolf -o brick_house.
      """.stripMargin

    test(r,
      "pig & pig & pig & bricks & sticks & straw & wolf",
      "brick_house").head should be (rules(r))
  }

  def rules(input: String): List[Expr] = {
    val Parsed.Success(rules, _) = Parser.parseProgram(input)
    rules.map(_.toExpr).toList
  }

  def test(rules: String, initialState: String, finalState: String): List[List[Expr]] = {
    val Parsed.Success(rules1, _) = Parser.parseProgram(rules)
    val Parsed.Success(state, _) = Parser.parseExpr(initialState)
    val Parsed.Success(goal, _) = Parser.parseExpr(finalState)

    Main.forward(rules1, state, goal)
  }

//  it should "throw NoSuchElementException if an empty stack is popped" in {
//    val emptyStack = new Stack[Int]
//    a [NoSuchElementException] should be thrownBy {
//      emptyStack.pop()
//    }
//  }
}
