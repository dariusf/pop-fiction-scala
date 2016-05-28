
import Core.Conj
import fastparse.core.Parsed
import org.scalatest._

class Tests extends FlatSpec with Matchers {

  "expression parser" should "munch maximally" in {
    def test(input: String): PExpr = {
      val Parsed.Success(result, _) = Parser.parseExpr(input)
      result
    }
    test("house & car") should be (PAnd(PTerm("house"), PTerm("car")))
  }

  "rules" should "be equal modulo ordering" in {
    rules("a & b -o c & d.") should be (rules("b & a -o d & c."))
  }

  "preprocessing" should "work correctly" in {

    def test(input: String): Conj = {
      val Parsed.Success(expr, _) = Parser.parseExpr(input)
      expr.preprocess(false)
    }

    test("a") should be (List(Term("a")))
    test("a & b") should be (List(Term("a"), Term("b")))
    test("a & b -o c") should be (List(
      Implies(List(Term("a"), Term("b")), List(Term("c")))))
    test("a | b -o c") should be (List(
        Implies(List(Term("a")), List(Term("c"))),
        Implies(List(Term("b")), List(Term("c")))))
    test("a | b") should be (List(
      Term("_1"),
      Implies(List(Term("_1")), List(Term("a"))),
      Implies(List(Term("_1")), List(Term("b"))),
      Implies(List(Term("a")), List(Term("_1"))),
      Implies(List(Term("b")), List(Term("_1")))))
  }

  "forward" should "work with transitive rules" in {
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
    ) should be (List(rules("a -o b.") ++ List(Implies(List(Term("b")), List(Term("_1"))))))
  }

  "conjunctions" should "not be affected by ordering" in {
    test(
      """
        |a & b -o c & d.
      """.stripMargin,
      "b & a",
      "d & c"
    ) should be (List(rules(
      """
        |b & a -o d & c.
      """.stripMargin)))
  }

  "forward" should "work with & on the left" in {
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
  }

  "forward" should "work with & on the right" in {
    test(
      """
        |a -o b & house.
      """.stripMargin,
      "a",
      "house & b"
    ) should be (List(rules(
      """
        |a -o b & house.
      """.stripMargin)))
  }

  "forward" should "work with rules with | on the left side" in {

    // These leak implementation details a bit...

    test(
      """
        |a | b -o house.
      """.stripMargin,
      "a",
      "house"
    ) should be (List(rules(
      """
        |a -o house.
      """.stripMargin)))

    test(
      """
        |a | b -o house.
      """.stripMargin,
      "b",
      "house"
    ) should be (List(rules(
      """
        | b -o house.
      """.stripMargin)))
  }

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
    rules.flatMap(_.toExpr).toList
  }

  def test(rules: String, initialState: String, finalState: String): List[Conj] = {
    val Parsed.Success(rules1, _) = Parser.parseProgram(rules)
    val Parsed.Success(state, _) = Parser.parseExpr(initialState)
    val Parsed.Success(goal, _) = Parser.parseExpr(finalState)

    Main.forward(rules1, state, goal)
  }
}
