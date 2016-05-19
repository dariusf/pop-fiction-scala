
import fastparse.WhitespaceApi

object Parser {

  val White = WhitespaceApi.Wrapper{
    import fastparse.all._
    NoTrace(P(CharIn(" \n\r")).rep)
  }

  import fastparse.noApi._
  import White._

  // CharIn('0' to '9')
  val term: P[PExpr] = P((CharIn('a' to 'z') | "_").repX(1).!).map(PTerm(_))
  val grouped: P[PExpr] = P(term | parens)
  val conj: P[PExpr] = P(grouped ~ ("&" ~/ grouped).rep)
    .map { case (x, xs) =>
      xs.foldLeft(x){ case (left, right) => PAnd(left, right) }
    }
  val disj: P[PExpr] = P(conj ~ ("|" ~/ conj).rep)
    .map { case (x, xs) =>
      xs.foldLeft(x){ case (left, right) => POr(left, right) }
    }
  val impl: P[PExpr] = P(disj ~ ("-o" ~/ impl).?)
    .map { case (x, xs) =>
      if (xs.isDefined) {
        PImplies(x, xs.get)
      } else {
        x
      }
    }
  val parens: P[PExpr] = P("(" ~/ impl ~ ")")
  val expr: P[PExpr] = impl
  val rule: P[PExpr] = P(impl ~ ".")
  val prog: P[Seq[PExpr]] = P(" ".rep ~ rule.rep ~ " ".rep ~ End)

  def parseProgram(input: String): Parsed[Seq[PExpr]] = {
    prog.parse(input, 0)
  }

  def parseExpr(input: String): Parsed[PExpr] = {
    expr.parse(input, 0)
  }

  def main(args: Array[String]) {
    //    System.out.println(prog.parse("a & (c | d) -o b.\nb -o c.", 0))
  }
}
