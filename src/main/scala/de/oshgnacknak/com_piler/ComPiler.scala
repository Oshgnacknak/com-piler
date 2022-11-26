package de.oshgnacknak.com_piler

import java.nio.file.{Files, Path}

object ComPiler {

  def main(args: Array[String]): Unit = {
    val s = "(((2 * 5) leq (1 + z)) or (4 eq (2 + z)))"

    val expr = Parser.parseExpression(s)
    val proof = new Proofer(_ => 2).proofAsBool(expr)
    println(proof)
  }

  def main2(args: Array[String]): Unit = {
    val src = Files.readString(Path.of("soos.com"))
    val ast = Parser.parse(src)
    TypeChecker.check(ast)
    println(LaTeXifyer.toLaTeX(ast))
  }
}
