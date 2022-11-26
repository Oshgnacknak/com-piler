package de.oshgnacknak.com_piler

import java.nio.file.{Files, Path}

object ComPiler {

  private val sigma = Map[String, Int](
    "x" -> 5
  )

  def main2(args: Array[String]): Unit = {
    val s = "(((2 * 5) leq (1 + z)) or (4 eq (2 + z)))"

    val expr = Parser.parseExpression(s)

    val proof = Proofer.proofAsBool(expr, sigma)
    println(proof)
  }

  def main(args: Array[String]): Unit = {
    val src = Files.readString(Path.of("saas.com"))
    val commando = Parser.parse(src)
    val proof = Proofer.proof(commando, sigma);
    println(proof)
  }
}
