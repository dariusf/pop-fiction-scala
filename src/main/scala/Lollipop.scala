
import fastparse.core.Parsed

import scala.scalajs.js.JSApp
import scala.scalajs.js.annotation.JSExport

object Lollipop extends JSApp {

  @JSExport
  def parse(input: String): Parsed[Seq[PExpr]] = {
    Parser.parseProgram(input)
  }

  @JSExport
  def run(rulesInput: String, initialState: String, finalState: String): String = {

    val Parsed.Success(rules, _) = Parser.parseProgram(rulesInput)
    val Parsed.Success(state, _) = Parser.parseExpr(initialState)
    val Parsed.Success(goal, _) = Parser.parseExpr(finalState)

    Main.prettyPrint(Main.runToGoal(rules, state, goal))
  }

  def main(): Unit = {
    println("Hello world!")
  }
}
