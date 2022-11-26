package de.oshgnacknak.com_piler

private class Parser(private var src: String) {

  def parse(): Commando = {
    val expr = parseCompound()

    if (src.nonEmpty) {
      throw new SyntaxError("Expected end of input, but got: " + src.head)
    }

    expr
  }

  def parseCompound(): Commando = {
    var c = parseCommando()

    skipWhitespace()
    while (hasAnyOf(";")) {
      acceptAnyOf(";")
      c = Compound(c, parseCommando())
      skipWhitespace()
    }

    c
  }

  def parseAssign(name: String): Commando = {
    acceptAnyOf(":=")
    Assign(name, parseExpression())
  }

  def parseCommando(): Commando = {
    skipWhitespace()
    val token = takeWhile(_.isLetterOrDigit)
    token match {
      case "skip" => Skip()
      case "while" => parseWhile()
      case "if" => parseIf()
      case _ => parseAssign(token)
    }
  }

  def parseIf(): Commando = {
    val cond = parseExpression()
    acceptAnyOf("then")
    val thenBody = parseCompound()
    acceptAnyOf("else")
    val elseBody = parseCompound()
    acceptAnyOf("fi")
    If(cond, thenBody, elseBody)
  }

  def parseWhile(): Commando = {
    val cond = parseExpression()
    acceptAnyOf("do")
    val body = parseCompound()
    acceptAnyOf("od")
    While(cond, body)
  }

  def parseExpression(): Expression = {
    skipWhitespace()
    val c = src.head

    if (c.isDigit) {
      return parseALit()
    }

    if (c.isLetter) {
      return parseIdentifier()
    }

    parseBinaryOp()
  }

  def parseBinaryOp(): Expression = {
    val t = acceptAnyOf("(")

    val left = parseExpression()
    val op = acceptAnyOf("+", "-", "*", "eq", "leq", "and", "or")
    val right = parseExpression()
    acceptAnyOf(")")

    op match {
      case "+" => Add(left, right)
      case "-" => Sub(left, right)
      case "*" => Mul(left, right)
      case "eq" => Equals(left, right)
      case "leq" => Leq(left, right)
      case "and" => And(left, right)
      case "or" => Or(left, right)
    }
  }

  def parseALit(): ALit = {
    val value = takeWhile(_.isDigit)
    ALit(value.toInt)
  }

  def parseIdentifier(): Expression = {
    val name = takeWhile(_.isLetterOrDigit)
    name match {
      case "true" => BLit(true)
      case "false" => BLit(false)
      case "not" => Not(parseExpression())
      case _ => Var(name)
    }
  }

  def takeWhile(p: Char => Boolean): String = {
    val (x, y) = src.span(p)
    src = y
    x
  }

  def skipWhitespace(): Unit = {
    src = src.trim
  }


  def acceptAnyOf(expected: String*): String = {
    skipWhitespace()
    if (src.isEmpty) {
      throw new SyntaxError("Unexpected end of input")
    }

    val token = expected
      .find(src.startsWith(_))
      .getOrElse(throw new SyntaxError(s"Expected any of `${expected.mkString(", ")}`, but got `${src.head}`"))

    src = src.substring(token.length)
    token
  }


  def hasAnyOf(s: String): Boolean = {
    src.nonEmpty && s.contains(src.head)
  }

}

object Parser {

  def parse(s: String): Commando = new Parser(s).parse()

  def parseExpression(s: String): Expression = new Parser(s).parseExpression()
}