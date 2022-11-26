package de.oshgnacknak.com_piler

class Proofer(sigma: String => Int) {

  def evalAsInt(expr: Expression): Int = {
    TypeChecker.expect(Type.Integer, expr)

    expr match {
      case ALit(value) => value
      case Var(name) => sigma(name)
      case Add(left, right) => evalAsInt(left) + evalAsInt(right)
      case Sub(left, right) => evalAsInt(left) - evalAsInt(right)
      case Mul(left, right) => evalAsInt(left) * evalAsInt(right)
    }
  }

  def evalAsBool(expr: Expression): Boolean = {
    TypeChecker.expect(Type.Boolean, expr)

    expr match {
      case BLit(value) => value
      case Equals(left, right) => evalAsInt(left) == evalAsInt(right)
      case Leq(left, right) => evalAsInt(left) <= evalAsInt(right)
      case Not(expr) => !evalAsBool(expr)
      case And(left, right) => evalAsBool(left) && evalAsBool(right)
      case Or(left, right) => evalAsBool(left) || evalAsBool(right)
    }
  }

  def proofAsInt(expr: Expression): String = {
    val res = evalAsInt(expr)
    val inner = s"< ${LaTeXifyer.toLaTeX(expr)}, \\sigma > \\Downarrow $res"

    expr match {
      case ALit(_) => s"\\infer[left label={$$rNum$$}]0{$inner}"
      case Var(name) => s"\\infer[left label={$$rVar$$}]0[$$\\sigma($name) = $res$$]{$inner}"
      case Add(left, right) =>
        proofAsInt(left) + "\n" +
        proofAsInt(right) + "\n" +
        s"\\infer[left label={$$r\\oplus$$}]2[$res = ${evalAsInt(left)} + ${evalAsInt(right)}]{$inner}"
      case Sub(left, right) =>
        proofAsInt(left) + "\n" +
        proofAsInt(right) + "\n" +
        s"\\infer[left label={$$r\\ominus$$}]2[$res = ${evalAsInt(left)} - ${evalAsInt(right)}]{$inner}"
      case Mul(left, right) =>
        proofAsInt(left) + "\n" +
        proofAsInt(right) + "\n" +
        s"\\infer[left label={$$r\\odot$$}]2[$res = ${evalAsInt(left)} * ${evalAsInt(right)}]{$inner}"
    }
  }

  def proofAsBool(expr: Expression): String = {
    val res = evalAsBool(expr)
    val inner = s"< ${LaTeXifyer.toLaTeX(expr)}, \\sigma > \\Downarrow $res"
    val t = res.toString.head

    expr match {
      case BLit(_) => s"\\infer[left label={$$r$res$$}]0{$inner}"
      case Equals(left, right) =>
        val neq = if (res) "" else "\\lnot"
        proofAsInt(left) + "\n" +
        proofAsInt(right) + "\n" +
        s"\\infer[left label={$$req$t$$}]2[$$$neq ${evalAsInt(left)} = ${evalAsInt(right)}$$]{$inner}"
      case Leq(left, right) =>
        val cmp = if (res) "\\leq" else ">"
        proofAsInt(left) + "\n" +
        proofAsInt(right) + "\n" +
        s"\\infer[left label={$$rleq$t$$}]2[$$${evalAsInt(left)} $cmp ${evalAsInt(right)}$$]{$inner}"
      case Not(expr) =>
        proofAsBool(expr) + "\n" +
        s"\\infer[left label={$$rnot$t$$}]1{$inner}"
      case And(left, right) =>
        if (res)
          proofAsBool(left) + "\n" +
          proofAsBool(right) + "\n" +
          s"\\infer[left label={$$randt$$}]2{$inner}"
        else if (!evalAsBool(left))
          proofAsBool(left) + "\n" +
          s"\\infer[left label={$$randf1$$}]1{$inner}"
        else
          proofAsBool(right) + "\n" +
          s"\\infer[left label={$$randf2$$}]1{$inner}"
      case Or(left, right) =>
        if (!res)
          proofAsBool(left) + "\n" +
          proofAsBool(right) + "\n" +
          s"\\infer[left label={$$rorf$$}]2{$inner}"
        else if (evalAsBool(left))
          proofAsBool(left) + "\n" +
          s"\\infer[left label={$$rort1$$}]1{$inner}"
        else
          proofAsBool(right) + "\n" +
          s"\\infer[left label={$$rort2$$}]1{$inner}"
    }
  }
}
