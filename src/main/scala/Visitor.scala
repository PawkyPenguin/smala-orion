package Visitors

import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import ASTTypes._

sealed trait Visitor[From, To] {
  def visit(a: From): To

  def visit[T <: From](seq: Seq[T], nullElement: To): To = {
        if (seq.isEmpty) {
          nullElement
        } else {
          var lastChild = nullElement
          for (e <- seq) {
            lastChild = visit(e)
          }
          lastChild
        }
  }
}

sealed class Optimizer extends Visitor[AST, AST] {
  override def visit(a: AST): AST = {
    ???
  }
}

sealed class EMIModifier(rand: scala.util.Random) extends Visitor[AST, AST] {
  override def visit(a: AST): AST = {
    a match {
      case e @ Prog(args, bodypart) => Prog(args, visit(bodypart).asInstanceOf[Expression])
      case e @ Coverage(body) => {
        if (!e.covered) {
          // decide whether we want to prune or not
          val nonPrunedExpressions: ListBuffer[Expression] = ListBuffer()
          val amountOfExpressions: Float = body.children.size
          for (expression <- body.children) {
            if (rand.nextFloat > 1.0 / amountOfExpressions) {
              nonPrunedExpressions += expression
            }
          }
          Coverage(Body(nonPrunedExpressions))
        } else {
          Coverage(visit(body).asInstanceOf[Body])
        }
      }
      case e: Body => Body(e.children.map(visit).asInstanceOf[Seq[Expression]])
      case Assignment(name, expr) => Assignment(name, visit(expr).asInstanceOf[Expression])
      case ValAssignment(name, expr) => ValAssignment(name, visit(expr).asInstanceOf[Expression])
      case VarAssignment(name, expr) => VarAssignment(name, visit(expr).asInstanceOf[Expression])
      case BinOp(left, right, op) => BinOp(visit(left).asInstanceOf[Expression], visit(right).asInstanceOf[Expression], op)
      case Condition(left, right, op) => Condition(visit(left).asInstanceOf[Expression], visit(right).asInstanceOf[Expression], op)
      case Branch(cond, body, elsebody) => Branch(visit(cond).asInstanceOf[Condition], visit(body).asInstanceOf[Expression], visit(elsebody).asInstanceOf[Expression])
      case x => x.cloneAST
    }
  }
}

sealed class CoverageVisitor extends Visitor[AST, AST] {
  override def visit(a: AST): AST = {
    a match {
      case e @ Prog(args, bodypart) => Prog(args, visit(bodypart).asInstanceOf[Expression])
      case e: Body => Coverage(Body(e.children.map(visit).asInstanceOf[Seq[Expression]]))
      case Assignment(name, expr) => Assignment(name, visit(expr).asInstanceOf[Expression])
      case ValAssignment(name, expr) => ValAssignment(name, visit(expr).asInstanceOf[Expression])
      case VarAssignment(name, expr) => VarAssignment(name, visit(expr).asInstanceOf[Expression])
      case BinOp(left, right, op) => BinOp(visit(left).asInstanceOf[Expression], visit(right).asInstanceOf[Expression], op)
      case Condition(left, right, op) => Condition(visit(left).asInstanceOf[Expression], visit(right).asInstanceOf[Expression], op)
      case Branch(cond, body, elsebody) => Branch(visit(cond).asInstanceOf[Condition], visit(body).asInstanceOf[Expression], visit(elsebody).asInstanceOf[Expression])
      case x => x.cloneAST
    }
  }
}

final case class SemanticCheckerException(private val message: String = "") extends Exception(message)

sealed class SemanticChecker extends Visitor[AST, String] {

  val variableTypes: Map[String, String] = Map()
  val valueTypes: Map[String, String] = Map()

  override def visit(a: AST): String = {
    a match {
      case e: Prog => visit(e.children, "Object")
      case e: BoolValue => "Boolean"
      case e: IntValue => "Int"
      case e: Deref =>
        try {
          variableTypes.getOrElse(e.name, valueTypes(e.name))
        } catch {
          case ex: Exception => 
            throw new SemanticCheckerException("Accessed variable before its definition")
        }
      case e: Assignment => {
        val exprType = visit(e.expr)
        variableTypes get e.variableName match {
          case Some(v) =>
            if (v == exprType) {
              exprType
            } else {
              throw new SemanticCheckerException("Rigth-hand side of assignment has wrong type")
            }
          case None =>
            throw new SemanticCheckerException("Tried to assign to val or undeclared variable")
        }
      }
      case e: VarAssignment =>
        if (variableTypes.contains(e.variableName) || valueTypes.contains(e.variableName)) {
          throw new SemanticCheckerException("Tried to declare variable, but value or variable is already declared")
        } else {
          val exprType = visit(e.expr)
          variableTypes(e.variableName) = visit(e.expr)
          exprType
        }
      case e: ValAssignment =>
        if (variableTypes.contains(e.variableName) || valueTypes.contains(e.variableName)) {
          throw new SemanticCheckerException("Tried to declare variable, but value or variable is already declared")
        } else {
          val exprType = visit(e.expr)
          valueTypes(e.variableName) = visit(e.expr)
          exprType
        }
      case e: BinOp =>
        if (visit(e.left) != "Int" || visit(e.right) != "Int") {
          throw new SemanticCheckerException("Expressions on both side of BinOp must be Int")
        } else {
          "Int"
        }
      case e: Condition =>
        if (visit(e.left) != "Int" || visit(e.right) != "Int") {
          println("left " + visit(e.left) + " right " + visit(e.right))
          throw new SemanticCheckerException("Expressions on both side of Condition must be Int")
        } else {
          "Boolean"
        }
      case e: Body => visit(e.children, "Object")
      case e: Branch =>
        if (visit(e.cond) != "Boolean") {
          throw new SemanticCheckerException("Condition of if-expression must be boolean")
        } else {
          val bodyType = visit(e.body)
          val elseType = visit(e.elseBranch)
          if (bodyType == elseType) {
            bodyType
          } else {
            "Object"
          }
        }
      case Coverage(b) => visit(b)
    }
  }
}

sealed class Simulator extends Visitor[AST, Either[Boolean, Int]] {
  type Value = Either[Boolean, Int]

  val varMap: Map[String, Value] = Map()
  val valMap: Map[String, Value] = Map()

  def simulate(arguments: Map[String, Value])(ast: AST): Value = {
    varMap.clear
    valMap.clear
    ast match {
      case Prog(parameterList: Seq[ValAssignment], body: Expression) => {
        val missingAssignments: ListBuffer[ValAssignment] = ListBuffer()
        for (assignment <- parameterList) {
          val passedInArgument = arguments.get(assignment.variableName)
          passedInArgument match {
            case Some(argumentValue) => valMap(assignment.variableName) = argumentValue
            case None => missingAssignments.append(assignment)
          }
        }
        val newAST = Prog(missingAssignments, body)
        val result = visit(Prog(missingAssignments, body))
        result
      }
      case a => visit(a)
    }
  }

  override def visit(a: AST): Value = {
    a match {
      case e: Prog => if (valMap.isEmpty) {
        visit(e.children, Left(false): Either[Boolean, Int])
      } else {
        visit(e.body)
      }
      case e: Coverage => {
        e.covered = true
        visit(e.children, Left(false): Either[Boolean, Int])
      }
      case e: BoolValue => Left(e.el)
      case e: IntValue => Right(e.el)
      case e: Deref => varMap.getOrElse(e.name, valMap(e.name))
      case e: Assignment =>
        if (varMap.contains(e.variableName)) {
          val expr = visit(e.expr)
          varMap(e.variableName) = expr
          expr
        } else {
          // Yup, that's me. You're probably wondering how I ended up in this situation.
          throw new IllegalStateException("Tried to assign to undeclared variable. This is likely a bug in the semantic checking step!")
        }
      case e: VarAssignment => {
        val expr = visit(e.expr)
        varMap(e.variableName) = expr
        expr
      }
      case e: ValAssignment => {
        val expr = visit(e.expr)
        valMap(e.variableName) = expr
        expr
      }
      case e: BinOp => Right(binOp(visit(e.left).right.get, visit(e.right).right.get, e.opSym))
      case e: Condition => Left(condOp(visit(e.left).right.get, visit(e.right).right.get, e.opSym))
      case e: Body => visit(e.children, Left(false): Either[Boolean, Int])
      case e: Branch =>
        if (visit(e.cond).left.get) {
          visit(e.body)
        } else {
          visit(e.elseBranch)
        }
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
      case e: Prog => "\ndef prog(" + e.args.map{visit}.mkString(", ") + ") = " + visit(e.body)
      case e: BoolValue => e.el.toString
      case e: IntValue => e.el.toString
      case e: Deref => e.name
      case e: Assignment => e.variableName + " = " + visit(e.expr)
      case e: ValAssignment => "val " + e.variableName + " = " + visit(e.expr)
      case e: VarAssignment => "var " + e.variableName + " = " + visit(e.expr)
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
      case e: Coverage => if (e.covered) {
        indent += 1
        val str = "{\n" + getIndent + "LIVE\n" + getIndent + visit(e.body) + "\n"
        indent -= 1
        str + getIndent + "}"
      } else {
        indent += 1
        val str = "{\n" + getIndent + "DEAD\n"
        indent -= 1
        str + getIndent + "}"
      }
    }
  }
}
