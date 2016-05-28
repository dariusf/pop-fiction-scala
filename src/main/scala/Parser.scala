
import fastparse.all._
//import fastparse.WhitespaceApi
//
object Parser {
//
//  val White = WhitespaceApi.Wrapper{
//    import fastparse.all._
//    NoTrace(P(CharIn(" \n\r")).rep)
//  }
//
//  import fastparse.noApi._
//  import White._

  def parenthesize(p: P[PExpr]): P[PExpr] =
    space ~ "(" ~ space ~/ p ~ space ~ ")" ~ space

  val space: P[Unit] = P(CharsWhile(NamedFunction(" \r\n".contains(_: Char), "Whitespace")).?)
  val ident: P[String] = P(CharIn('a' to 'z') ~ (CharIn('a' to 'z') | "_").rep).!
  val varName: P[String] = P(CharIn('A' to 'Z') ~ (CharIn('a' to 'z') | "_").rep).!

  val variable: P[PExpr] = varName.map(PVar(_))
  val term: P[PExpr] = ident.map(PTerm(_))
  val compound: P[PExpr] = P(ident ~ space ~
    (variable | term | parenthesize(compound | term | variable)).rep (sep=space) ~ space).map {
    case (name, args) =>
      if (args.isEmpty) {
        PTerm(name)
      } else {
        PCompound(name, args.toList)
      }
  }
  val grouped: P[PExpr] = P(compound | term | variable | parens)
  val conj: P[PExpr] = P(grouped ~ (space ~ "&" ~ space ~/ grouped).rep)
    .map { case (x, xs) =>
      xs.foldLeft(x){ case (left, right) => PAnd(left, right) }
    }
  val disj: P[PExpr] = P(conj ~ (space ~ "|" ~ space ~/ conj).rep)
    .map { case (x, xs) =>
      xs.foldLeft(x){ case (left, right) => POr(left, right) }
    }
  val impl: P[PExpr] = P(disj ~ (space ~ "-o" ~ space ~/ impl).?)
    .map { case (x, xs) =>
      if (xs.isDefined) {
        PImplies(x, xs.get)
      } else {
        x
      }
    }
  val parens: P[PExpr] = parenthesize(impl)
  val expr: P[PExpr] = space ~ impl ~ space
  val rule: P[PExpr] = P(impl ~ ".")
  val prog: P[Seq[PExpr]] = space ~ rule.rep ~ space ~ End

  case class NamedFunction[T, V](f: T => V, name: String) extends (T => V) {
    def apply(t: T) = f(t)
    override def toString() = name
  }

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
