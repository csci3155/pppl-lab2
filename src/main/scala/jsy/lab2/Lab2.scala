package jsy.lab2

object Lab2 extends jsy.util.JsyApplication {
  import ast._
  /*
   * CSCI 3155: Lab 2
   * <Your Name>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case _    => ???
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case _    => ???
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s)      => s
      case Undefined => "undefined"
      case _         => ???
    }
  }

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   *
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */
  type Env = Map[String, Expr]
  val empty: Env = Map()
  def lookup(env: Env, x: String): Expr = env(x)
  def extend(env: Env, x: String, v: Expr): Env = {
    require(isValue(v))
    env + (x -> v)
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case _ => ???
    }
  }

  /* Interface to run your interpreter starting from an empty environment. This is convenient for unit testing.  */
  def eval(e: Expr): Expr = {
    require(closed(e))
    eval(empty, e)
  }

  /* Interface to run your interpreter from a string. This is convenient for unit testing.  */
  def eval(s: String): Expr = eval(Parser.parse(s))

  // You may set this debug flag to true to get the debugging output below, but you need to turn it off to pass test cases.
  // debug = true

  /* Interface to run your interpreter on an input file. This is for file-based integration tests (see Lab2JsyTests in Lab2Spec.scala). */
  def processFile(file: java.io.File): Unit = {

    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(pretty(v))
  }
}
