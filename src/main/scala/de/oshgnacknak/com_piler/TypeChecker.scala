package de.oshgnacknak.com_piler

object TypeChecker {

  def check(commando: Commando): Unit = {
    commando match {
      case Skip() =>
      case Assign(_, expr) =>
        expect(Type.Integer, expr)
      case Compound(left, right) =>
        check(left)
        check(right)
      case If(cond, thenBody, elseBody) =>
        expect(Type.Boolean, cond)
        check(thenBody)
        check(elseBody)
      case While(cond, body) =>
        expect(Type.Boolean, cond)
        check(body)
    }
  }

  def expect(expected: Type.Value, expr: Expression): Unit = {
    val actual = expr match {
      case BLit(_) => Type.Boolean
      case Equals(left, right) =>
        expect(Type.Integer, left)
        expect(Type.Integer, right)
        Type.Boolean
      case Leq(left, right) =>
        expect(Type.Integer, left)
        expect(Type.Integer, right)
        Type.Boolean
      case Not(expr) =>
        expect(Type.Boolean, expr)
        Type.Boolean
      case And(left, right) =>
        expect(Type.Boolean, left)
        expect(Type.Boolean, right)
        Type.Boolean
      case Or(left, right) =>
        expect(Type.Boolean, left)
        expect(Type.Boolean, right)
        Type.Boolean
      case ALit(_) => Type.Integer
      case Var(name) => Type.Integer
      case Add(left, right) =>
        expect(Type.Integer, left)
        expect(Type.Integer, right)
        Type.Integer
      case Sub(left, right) =>
        expect(Type.Integer, left)
        expect(Type.Integer, right)
        Type.Integer
      case Mul(left, right) =>
        expect(Type.Integer, left)
        expect(Type.Integer, right)
        Type.Integer
    }

    if (actual != expected) {
      throw new TypeError(s"Expected $expected, but got $actual")
    }
  }
}
