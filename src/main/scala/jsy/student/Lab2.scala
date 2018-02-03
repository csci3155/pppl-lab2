package jsy.student

import jsy.lab2.Lab2Like

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <NAME>
   * 
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))        //set the requirement for val (to be v)
    (v: @unchecked) match {
      case N(n) => n    //if it exists as a number; return n
      case B(true) => 1.0    //if true;  return 1.0
      case B(false) => 0.0    //if false; return 0.0
      case S(s) => try s.toDouble catch {case _: Throwable => Double.NaN}   // attempt string (to)-> double
      //otherwise; NaN
      case Undefined => Double.NaN   //if Undef; NaN
      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))       //set requirement
    (v: @unchecked) match {
      case B(b) => b    //simple initial test case. If it is a boolean; return it
      case N(0.0) => false    //if N has value 0.0 - it is false
        // any other number will be true
      case N(n) => { if (n.isNaN) false else true }    //if NaN; return false
      //otherwise return true
      case S(s) => if (s == "") false else true      //if strings are empty; return false
      //otherwise return true; a non zero value
      case Undefined => false    //catch all for an error or undef, returning false such as if it had a 0.0 val
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))        //set requirement
    (v: @unchecked) match {
      case S(s) => s          //initial test case. If string; return
      case N(n) => prettyNumber(n)    //checks for if a whole number or NaN and so on
      case B(b) => {      //take a boolean and returns it as a string value
        if (b) "true"    //if b classifies as a boolean (?)
        else "false"     //otherwise; doesn't work and return false
      }
      case Undefined => "undefined"     //output undefined; "Undefined" is a system interpretation
      case _ => throw new UnsupportedOperationException    //catch all final case
    }
  }


  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(n) => N(n)   //base for number
      case S(s) => S(s)   //base for string
      case B(b) => B(b)   //base for boolean

      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined    //print
      case Binary(Plus, N(n1), N(n2)) => N(n1+n2)      //plus operation
      case Binary(Minus, N(n1), N(n2)) => N(n1-n2)     //minus operation
      case Binary(Times, N(n1), N(n2)) => N(n1*n2)     //multiplication operation
      case Binary(Div, N(n1), N(n2)) => N(n1/n2)       //division operation
      case _ => ???
    }
  }

  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
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

