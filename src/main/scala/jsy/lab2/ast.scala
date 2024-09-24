package jsy.lab2

import scala.util.parsing.input.Positional

/**
  * @author Bor-Yuh Evan Chang
  */
object ast {
  sealed trait Expr extends Positional // e ::=

  /* Literals */
  case class N(n: Double) extends Expr // e ::= n
  case class B(b: Boolean) extends Expr // e ::= b
  case class S(str: String) extends Expr // e ::= str
  case object Undefined extends Expr // e ::= undefined

  /* Unary and Binary Expressions */
  case class Unary(uop: Uop, e1: Expr) extends Expr // e ::= uop e1
  case class Binary(bop: Bop, e1: Expr, e2: Expr) extends Expr // e ::= e1 bop e2

  sealed trait Uop // uop ::=
  sealed trait Bop // bop ::=

  /* Numbers */
  case object Neg extends Uop // uop ::= -

  case object Plus extends Bop // bop ::= +
  case object Minus extends Bop // bop ::= -
  case object Times extends Bop // bop ::= *
  case object Div extends Bop // bop ::= /

  /* Booleans */
  case object Not extends Uop // uop ::= !
  case object And extends Bop // bop ::= &&
  case object Or extends Bop // bop ::= ||

  case class If(e1: Expr, e2: Expr, e3: Expr) extends Expr // e ::= e1 ? e2 : e3

  case object Eq extends Bop // bop ::= ===
  case object Ne extends Bop // bop ::= !==
  case object Lt extends Bop // bop ::= <
  case object Le extends Bop // bop ::= <=
  case object Gt extends Bop // bop ::= >
  case object Ge extends Bop // bop ::= >=

  /* Side-Effects */
  case class Print(e1: Expr) extends Expr // e ::= console.log(e1)
  case object Seq extends Bop // bop ::= ,

  /* Variables */
  case class Var(x: String) extends Expr // e ::= x
  case class ConstDecl(x: String, e1: Expr, e2: Expr) extends Expr // e ::= const x = e1; e2

  /* Define values. */
  def isValue(e: Expr): Boolean = e match {
    case N(_) | B(_) | S(_) | Undefined => true
    case _                              => false
  }

  /*
   * Pretty-print values.
   *
   * We do not override the toString method so that the abstract syntax can be printed
   * as is.
   */
  def pretty(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n)      => prettyNumber(n)
      case B(b)      => b.toString
      case S(str)    => str
      case Undefined => "undefined"
    }
  }

  def prettyNumber(n: Double): String =
    if (n.isWhole) "%.0f" format n else n.toString

  /* Get the free variables of e. */
  def freeVars(e: Expr): Set[String] = e match {
    case Var(x)                         => Set(x)
    case ConstDecl(x, e1, e2)           => freeVars(e1) | (freeVars(e2) - x)
    case N(_) | B(_) | Undefined | S(_) => Set.empty
    case Unary(_, e1)                   => freeVars(e1)
    case Binary(_, e1, e2)              => freeVars(e1) | freeVars(e2)
    case If(e1, e2, e3) => freeVars(e1) | freeVars(e2) | freeVars(e3)
    case Print(e1)      => freeVars(e1)
  }

  /* Check closed expressions. */
  def closed(e: Expr): Boolean = freeVars(e).isEmpty
}
