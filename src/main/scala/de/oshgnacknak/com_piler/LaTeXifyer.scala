package de.oshgnacknak.com_piler

case object LaTeXifyer {

  def toLaTeX(expr: Expression): String = expr match {
    case BLit(value) => value.toString
    case Equals(left, right) => s"(${toLaTeX(left)}~eq~${toLaTeX(right)})"
    case Leq(left, right) => s"(${toLaTeX(left)}~leq~${toLaTeX(right)})"
    case Not(expr) => "not~" + toLaTeX(expr)
    case And(left, right) => s"(${toLaTeX(left)}~and~${toLaTeX(right)})"
    case Or(left, right) => s"(${toLaTeX(left)}~or~${toLaTeX(right)})"
    case ALit(value) => value.toString
    case Var(name) => name
    case Add(left, right) => s"(${toLaTeX(left)}~\\oplus~${toLaTeX(right)})"
    case Sub(left, right) => s"(${toLaTeX(left)}~\\ominus~${toLaTeX(right)})"
    case Mul(left, right) => s"(${toLaTeX(left)}~\\odot~${toLaTeX(right)})"
  }

  def toLaTeX(commando: Commando, indent: Int = 0): String = {
    val beginOfLine = "&" + "~".repeat(indent)
    val endOfLine = "\\\\"

    commando match {
      case Skip() => beginOfLine + "skip"
      case Assign(name, expr) => s"$beginOfLine$name := ${toLaTeX(expr)}"
      case Compound(left, right) =>
        s"""${toLaTeX(left, indent)}; $endOfLine
           |${toLaTeX(right, indent)}""".stripMargin
      case If(cond, thenBody, elseBody) =>
        s"""$beginOfLine\\code{if}~${toLaTeX(cond)}~\\code{then} $endOfLine
           |${toLaTeX(thenBody, indent + 4)} $endOfLine
           |$beginOfLine\\code{else} $endOfLine
           |${toLaTeX(elseBody, indent + 4)} $endOfLine
           |$beginOfLine\\code{fi}""".stripMargin
      case While(cond, body) =>
        s"""$beginOfLine\\code{while}~${toLaTeX(cond)}~\\code{do} $endOfLine
           |${toLaTeX(body, indent + 4)} $endOfLine
           |$beginOfLine\\code{od}""".stripMargin
    }
  }
}
