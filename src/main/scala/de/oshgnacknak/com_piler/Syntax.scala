package de.oshgnacknak.com_piler

object Type extends Enumeration {
  type Type = Value
  val Boolean, Integer = Value
}

sealed abstract class Expression()

case class BLit(value: Boolean) extends Expression()

case class Equals(left: Expression, right: Expression) extends Expression()

case class Leq(left: Expression, right: Expression) extends Expression()

case class Not(expr: Expression) extends Expression()

case class And(left: Expression, right: Expression) extends Expression()

case class Or(left: Expression, right: Expression) extends Expression()

case class ALit(value: Int) extends Expression()

case class Var(name: String) extends Expression()

case class Add(left: Expression, right: Expression) extends Expression()

case class Sub(left: Expression, right: Expression) extends Expression()

case class Mul(left: Expression, right: Expression) extends Expression()

sealed trait Commando

case class Skip() extends Commando

case class Assign(name: String, expr: Expression) extends Commando

case class Compound(left: Commando, right: Commando) extends Commando

case class If(cond: Expression, thenBody: Commando, elseBody: Commando) extends Commando

case class While(cond: Expression, body: Commando) extends Commando
