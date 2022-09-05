/*
 * CSCI 3155: Lab 2 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab2.scala.
 */

// Imports the parse and parseFile functions
import jsy.lab2.Parser.{parse, parseFile}

// Imports the ast nodes
import jsy.lab2.ast._

// Imports all of the functions from your implementation
import jsy.student.Lab2._

// Call the JavaScripty parser (from the provided library) on a string
val negFourAST = parse("-4")

// Construct an AST directly
val negFiveAST: Expr = Unary(Neg, N(5.0))

// Evaluate that JavaScripty expression.
//eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("undefined + 1")

// Parse the JavaScripty expression in your worksheet
val worksheetJsy = parseFile("src/main/scala/jsy/student/Lab2.worksheet.js")

// Eval the JavaScripty expression in your worksheet
//eval(worksheetJsy)