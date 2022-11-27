package de.oshgnacknak.com_piler

object Proofer {

  def evalAsInt(expr: Expression, sigma: Map[String, Int]): Int = {
    TypeChecker.expect(Type.Integer, expr)

    expr match {
      case ALit(value) => value
      case Var(name) => sigma(name)
      case Add(left, right) => evalAsInt(left, sigma) + evalAsInt(right, sigma)
      case Sub(left, right) => evalAsInt(left, sigma) - evalAsInt(right, sigma)
      case Mul(left, right) => evalAsInt(left, sigma) * evalAsInt(right, sigma)
    }
  }

  def evalAsBool(expr: Expression, sigma: Map[String, Int]): Boolean = {
    TypeChecker.expect(Type.Boolean, expr)

    expr match {
      case BLit(value) => value
      case Equals(left, right) => evalAsInt(left, sigma) == evalAsInt(right, sigma)
      case Leq(left, right) => evalAsInt(left, sigma) <= evalAsInt(right, sigma)
      case Not(expr) => !evalAsBool(expr, sigma)
      case And(left, right) => evalAsBool(left, sigma) && evalAsBool(right, sigma)
      case Or(left, right) => evalAsBool(left, sigma) || evalAsBool(right, sigma)
    }
  }

  def proof(c: Commando, sigma: Map[String, Int]): String = {
    val (p, _, _) = proofRec(c, 0, sigma)
    p
  }

  def proofRec(commando: Commando, state: Int, sigma: Map[String, Int]): (String, Int, Map[String, Int]) = {
    val sigmaTex = "\\sigma" + "'".repeat(state)
    val inner = s"< ${LaTeXifyer.toLaTeX(commando)}, $sigmaTex > \\to $sigmaTex'"

    commando match {
      case Skip() => (s"\\infer[left label={$$rsk$$}]0{$inner}", state, sigma)
      case Assign(name, expr) =>
        val r = evalAsInt(expr, sigma)
        val p = proofAsInt(expr, sigma) + "\n" +
          s"\\infer[left label={$$r:=$$}]1[{$$$sigmaTex' = $sigmaTex[$name \\backslash $r]$$}]{$inner}"
        (p, state, sigma.updated(name, r))
      case Compound(left, right) =>
        val (p1, state1, sigma1) = proofRec(left, state, sigma)
        val (p2, state2, sigma2) = proofRec(right, state1+1, sigma1)
        val sigmaNew = "\\sigma" + "'".repeat(state2)
        val p = s"$p1\n$p2\n" +
          s"\\infer[left label={$$r;$$}]2{< ${LaTeXifyer.toLaTeX(commando)}, $sigmaTex > \\to $sigmaNew'}"
        (p, state2, sigma2)
      case If(cond, thenBody, elseBody) =>
        val res = evalAsBool(cond, sigma)
        val t = res.toString.head
        val pc = proofAsBool(cond, sigma)
        val (pb, state1, sigma1) = proofRec(if (res) thenBody else elseBody, state, sigma)
        val sigmaNew = "\\sigma" + "'".repeat(state1)
        val p = s"\\infer[left label={$$rif$t$$}]2{< ${LaTeXifyer.toLaTeX(commando)}, $sigmaTex > \\to $sigmaNew'}"
        (s"$pc\n$pb\n$p", state1, sigma1)
      case wh @ While(cond, body) =>
        val res = evalAsBool(cond, sigma)
        val pc = proofAsBool(cond, sigma)
        if (!res) {
          val p = s"\\infer[left label={$$rwhf$$}]1{< ${LaTeXifyer.toLaTeX(commando)}, $sigmaTex > \\to $sigmaTex}"
          (s"$pc\n$p", state, sigma)
        } else {
          val (pb, state1, sigma1) = proofRec(body, state, sigma)
          val (pn, state2, sigma2) = proofRec(wh, state1, sigma1)
          val sigmaNew = "\\sigma" + "'".repeat(state2)
          val p = s"\\infer[left label={$$rwht$$}]3{< ${LaTeXifyer.toLaTeX(commando)}, $sigmaTex > \\to $sigmaNew'}"
          (s"$pc\n$pb\n$pn\n$p", state2, sigma2)
        }
    }
  }

  def proofAsInt(expr: Expression, sigma: Map[String, Int]): String = {
    val res = evalAsInt(expr, sigma)
    val inner = s"< ${LaTeXifyer.toLaTeX(expr)}, \\sigma > \\Downarrow $res"

    expr match {
      case ALit(_) => s"\\infer[left label={$$rNum$$}]0{$inner}"
      case Var(name) => s"\\infer[left label={$$rVar$$}]0[$$\\sigma($name) = $res$$]{$inner}"
      case Add(left, right) =>
        proofAsInt(left, sigma) + "\n" +
          proofAsInt(right, sigma) + "\n" +
          s"\\infer[left label={$$r\\oplus$$}]2[$$$res = (${evalAsInt(left, sigma)}) + (${evalAsInt(right, sigma)})$$]{$inner}"
      case Sub(left, right) =>
        proofAsInt(left, sigma) + "\n" +
          proofAsInt(right, sigma) + "\n" +
          s"\\infer[left label={$$r\\ominus$$}]2[$$$res = (${evalAsInt(left, sigma)}) - (${evalAsInt(right, sigma)})$$]{$inner}"
      case Mul(left, right) =>
        proofAsInt(left, sigma) + "\n" +
          proofAsInt(right, sigma) + "\n" +
          s"\\infer[left label={$$r\\odot$$}]2[$$$res = (${evalAsInt(left, sigma)}) \\cdot (${evalAsInt(right, sigma)})$$]{$inner}"
    }
  }

  def proofAsBool(expr: Expression, sigma: Map[String, Int]): String = {
    val res = evalAsBool(expr, sigma)
    val inner = s"< ${LaTeXifyer.toLaTeX(expr)}, \\sigma > \\Downarrow $res"
    val t = res.toString.head

    expr match {
      case BLit(_) => s"\\infer[left label={$$r$res$$}]0{$inner}"
      case Equals(left, right) =>
        val neq = if (res) "" else "\\lnot"
        proofAsInt(left, sigma) + "\n" +
          proofAsInt(right, sigma) + "\n" +
          s"\\infer[left label={$$req$t$$}]2[$$$neq ${evalAsInt(left, sigma)} = ${evalAsInt(right, sigma)}$$]{$inner}"
      case Leq(left, right) =>
        val cmp = if (res) "\\leq" else ">"
        proofAsInt(left, sigma) + "\n" +
          proofAsInt(right, sigma) + "\n" +
          s"\\infer[left label={$$rleq$t$$}]2[$$${evalAsInt(left, sigma)} $cmp ${evalAsInt(right, sigma)}$$]{$inner}"
      case Not(expr) =>
        proofAsBool(expr, sigma) + "\n" +
          s"\\infer[left label={$$rnot$t$$}]1{$inner}"
      case And(left, right) =>
        if (res)
          proofAsBool(left, sigma) + "\n" +
            proofAsBool(right, sigma) + "\n" +
            s"\\infer[left label={$$randt$$}]2{$inner}"
        else if (!evalAsBool(left, sigma))
          proofAsBool(left, sigma) + "\n" +
            s"\\infer[left label={$$randf1$$}]1{$inner}"
        else
          proofAsBool(right, sigma) + "\n" +
            s"\\infer[left label={$$randf2$$}]1{$inner}"
      case Or(left, right) =>
        if (!res)
          proofAsBool(left, sigma) + "\n" +
            proofAsBool(right, sigma) + "\n" +
            s"\\infer[left label={$$rorf$$}]2{$inner}"
        else if (evalAsBool(left, sigma))
          proofAsBool(left, sigma) + "\n" +
            s"\\infer[left label={$$rort1$$}]1{$inner}"
        else
          proofAsBool(right, sigma) + "\n" +
            s"\\infer[left label={$$rort2$$}]1{$inner}"
    }
  }
}
