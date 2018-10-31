package Visitors

import scala.collection.mutable.Map
import ASTTypes._

sealed trait Visitor[From, To] {
  def visit(a: From): To
}

sealed class Optimizer extends Visitor[AST, AST]{
  override def visit(a: AST): AST = {
    ???
  }
}

sealed class Coverage extends Visitor[AST, CoverageAST] {
  override def visit(a: AST): CoverageAST = {
    a match {
      case e: Body => {
        CoveragedBody(e.children.map(visit))
      }
      case x => x.cloneAST
    }
  }
}

sealed class Simulator extends Visitor[CoverageAST, Either[Boolean, Int]] {
  type Value = Either[Boolean, Int]

  val varMap: Map[String, Value] = Map()

  override def visit(a: CoverageAST): Value = {
    a match {
      case e: CoveragedBody => {
        e.covered = true
        e.children.foldLeft(Left(false): Either[Boolean, Int]){(_, child) => 
          visit(child)
        }
      }
      case e: BoolValue => Left(e.el)
      case e: IntValue => Right(e.el)
      case e: Variable => varMap(e.name)
      case e: Assignment => {
        varMap(e.variableName) = visit(e.expr)
        Left(false)
      }
      case e: BinOp => Right(binOp(visit(e.left).right.get, visit(e.right).right.get, e.opSym))
      case e: Condition => Left(condOp(visit(e.left).right.get, visit(e.right).right.get, e.opSym))
      case e: Body => e.children.foldLeft(Left(false): Either[Boolean, Int]){(_, child) => 
        visit(child)
      }
      case e: Branch => 
        if (visit(e.cond).left.get) {
          visit(e.body)
        } else {
          visit(e.elseBranch)
        }
      case e: Empty => Left(false)
    }
  }

  def binOp(e1: Int, e2: Int, opSym: Char) = {
    opSym match {
      case '+' => e1 + e2
      case '-' => e1 - e2
      case '*' => e1 * e2
      case '/' => e1 / 2
    }
  }

  def condOp(e1: Int, e2: Int, opSym: String) = {
    opSym match {
      case "<" => e1 < e2
      case ">" => e1 > e2
      case ">=" => e1 >= e2
      case "<=" => e1 <= e2
      case "==" => e1 == e2
    }
  }
}

sealed class Flattener extends Visitor[AST, String] {
  var indent = 0

  def getIndent = "\t" * indent

  override def visit(a: AST): String = {
    a match {
      case e: BoolValue => e.el.toString
      case e: IntValue => e.el.toString
      case e: Variable => e.name
      case e: Assignment => "val " + e.variableName + " = " + visit(e.expr)
      case e: BinOp => "(" + visit(e.left) + " " + e.opSym + " " + visit(e.right) + ")"
      case e: Condition => visit(e.left) + " " + e.opSym + " " + visit(e.right)
      case e: Body => {
        indent += 1
        val body: Seq[String] = e.children.map {el =>
          getIndent + visit(el) + "\n"
        }
        indent -= 1
        "{\n" + body.mkString + getIndent + "}"
      }
      case e: Branch => "if (" + visit(e.cond) + ") " + visit(e.body) + " else " + visit(e.elseBranch)
      case e: Empty => ""
    }
  }
}
