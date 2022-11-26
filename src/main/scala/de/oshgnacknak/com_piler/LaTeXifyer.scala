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

  def toLaTeX(commando: Commando): String = {
    commando match {
      case Skip() => "skip"
      case Assign(name, expr) => s"$name := ${toLaTeX(expr)}"
      case Compound(left, right) =>
        s"${toLaTeX(left)};~${toLaTeX(right)}"
      case If(cond, thenBody, elseBody) =>
        s"\\code{if}~${toLaTeX(cond)}~\\code{then}~${toLaTeX(thenBody)}~\\code{else}~${toLaTeX(elseBody)}~\\code{fi}"
      case While(cond, body) =>
        s"\\code{while}~${toLaTeX(cond)}~\\code{do}~${toLaTeX(body)}~\\code{od}"
    }
  }
}
