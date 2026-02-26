package popl

object hw02 extends js.util.JsApp:

  import js._
  import js.ast._
  import Expr._, Bop._, Uop._

  /*
   * CSCI-UA.0480-055: Homework 2
   */

  /*
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your solution will _not_ be graded if it does not compile!!
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   *
   */

  /* Problem 1: Binary Search Tree */

  enum Tree:
    case Empty
    case Node(left: Tree, data: Int, right: Tree)
  end Tree

  import Tree._

  def repOk(t: Tree): Boolean =
    def check(t: Tree, min: Int, max: Int): Boolean = t match
      case Empty => true
      case Node(left, data, right) =>
        min < data && data < max &&
          check(left, data, max) &&
          check(right, min, data)

    check(t, Int.MinValue, Int.MaxValue)

  def insert(t: Tree, n: Int): Tree = t match
    case Empty => Node(Empty, n, Empty)
    case Node(left, data, right) =>
      n.compareTo(data) match
        case 0 => t
        case 1 => Node(insert(left, n), data, right)
        case _ => Node(left, data, insert(right, n))

  def deleteMin(t: Tree): (Tree, Int) =
    require(t != Empty)
    (t: @unchecked) match
      case Node(left, data, Empty) => (left, data)
      case Node(left, data, right) =>
        val (right1, min) = deleteMin(right)
        (Node(left, data, right1), min)

  def delete(t: Tree, n: Int): Tree = t match
    case Empty => Empty
    case Node(left, data, right) =>
      if n > data then Node(delete(left, n), data, right)
      else if n < data then Node(left, data, delete(right, n))
      else if left == Empty then right
      else
        val (left1, min) = deleteMin(left)
        Node(left1, min, right)

  /* Problem 2: JakartaScript */

  def eval(e: Expr): Double = e match
    case Num(n) => n
    case UnOp(UMinus, e) => -eval(e)
    case BinOp(op, e1, e2) =>
      val n1 = eval(e1)
      val n2 = eval(e2)
      op match
        case Plus => n1 + n2
        case Minus => n1 - n2
        case Times => n1 * n2
        case Div => n1 / n2



  // Interface to run your interpreter from a string.  This is convenient
  // for unit testing.
  def eval(s: String): Double = eval(parse.fromString(s))


  /* Interface to run your interpreter from the command line.  You can ignore the code below. */

  def processFile(file: java.io.File): Unit =
    if debug then
      println("============================================================")
      println("File: " + file.getName)
      println("Parsing ...")

    val expr = handle(fail()) {
      parse.fromFile(file)
    }

    if debug then
      println("Parsed expression:")
      println(expr)

    handle(()) {
      val v = eval(expr)
      println(v)
    }

