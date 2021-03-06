
import Core.Conj
import Main.Sub
import fastparse.core.Parsed
import org.scalatest._

class Tests extends FlatSpec with Matchers {

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
    test("""
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

  "forward" should "work with variables" in {
    test("A -o b.",
      "a",
      "b"
    ) should be (List(rules("a -o b.")))
  }

  "forward" should "work for simple examples" in {
    test("at A B & at B C -o at B C & at A C.",
      "at a b & at b c",
      "at a c & at b c"
    ) should be (List(rules("at a b & at b c -o at b c & at a c.")))
  }

  "forward" should "work for... harder... examples" in {
    test("eros A B & eros Witness A -o eros A B & eros B A & anger Witness A & anger Witness B.",
      "eros desdemona cassio & eros othello desdemona",
      "eros desdemona cassio & eros cassio desdemona & anger othello desdemona & anger othello cassio"
    ) should be (List(rules("eros desdemona cassio & eros othello desdemona -o eros desdemona cassio & eros cassio " +
      "desdemona & anger othello desdemona & anger othello cassio.")))
  }

  "forward" should "work with parametric rules" in {
    val initialRules = """
        |pig & material A -o house A.
        |wolf & house straw -o wolf.
        |wolf & house stick -o wolf.
        |wolf & house brick -o house brick.
      """.stripMargin
    val finalRules =
      """
        |pig & material A -o house A.
        |pig & material A -o house A.
        |pig & material A -o house A.
        |wolf & house straw -o wolf.
        |wolf & house stick -o wolf.
        |wolf & house brick -o house brick.
      """.stripMargin
    test(initialRules,
      "pig & pig & pig & material straw & material brick & material stick & wolf",
      "house brick").head should be
        List(rules(finalRules))
  }

  "whitespace" should "should be allowed everywhere" in {
    parseExpr(" a & V | c D ( e E ) f & ( b & d ) -o d ") should be (
      PImplies(
        POr(
          PAnd(PTerm("a"), PVar("V")),
          PAnd(PCompound("c",
            List(PVar("D"),
            PCompound("e", List(PVar("E"))),
              PTerm("f"))),
            PAnd(PTerm("b"), PTerm ("d")))),
        PTerm("d")))
  }

  "unification" should "work" in {
    def test(left: Expr, right: Expr): List[Sub] =
      Main.unify(left, right)

    test(Var("A"), Term("a")) should be (List(Sub("A", Term("a"))))
  }

  "expression parser" should "munch maximally" in {
    parseExpr("house & car") should be (PAnd(PTerm("house"), PTerm("car")))
    parseExpr("A & car") should be (PAnd(PVar("A"), PTerm("car")))
  }

  "expression parser" should "accept named rules (optionally)" in {
    parseProgram("rule : house -o car.") should be (List(PImplies(PTerm("house"), PTerm("car"))))
    parseProgram("house -o car.") should be (parseProgram("rule : house -o car."))
  }

  "expression parser" should "accept terms and variables" in {
    parseExpr("a'") should be (PTerm("a'"))
    parseExpr("A'") should be (PVar("A'"))
  }

  "expression parser" should "accept conjunction" in {
    parseExpr("a & b") should be (PAnd(PTerm("a"), PTerm("b")))
    parseExpr("a, b") should be (parseExpr("a & b"))
  }

  def rules(input: String): List[Expr] = {
    val Parsed.Success(rules, _) = Parser.parseProgram(input)
    rules.flatMap(_.toExpr).toList
  }

  def parseProgram(input: String): List[PExpr] = {
    val Parsed.Success(result, _) = Parser.parseProgram(input)
    result.toList
  }

  def parseExpr(input: String): PExpr = {
    val Parsed.Success(result, _) = Parser.parseExpr(input)
    result
  }

  def test(rules: String, initialState: String, finalState: String): List[Conj] = {
    val Parsed.Success(rules1, _) = Parser.parseProgram(rules)
    val Parsed.Success(state, _) = Parser.parseExpr(initialState)
    val Parsed.Success(goal, _) = Parser.parseExpr(finalState)

    Main.runToGoal(rules1, state, goal)
  }
}
