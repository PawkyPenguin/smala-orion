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

sealed class ConstantFolder extends Visitor[AST, AST] {
  type Value = Either[BoolValue, IntValue]

  var constantValues: Map[String, Value] = Map()

  override def visit(a: AST): AST = {
    def evalBool(l: BoolValue, r: BoolValue, opSym: String): BoolValue = {
      opSym match {
        case "||" => BoolValue(l.el || r.el)
        case "&&" => BoolValue(l.el && r.el)
      }
    }

    def evalInt(l: IntValue, r: IntValue, opSym: String) = {
      opSym match {
        case "+" => IntValue(l.el + r.el)
        case "-" => IntValue(l.el - r.el)
        case "*" => IntValue(l.el * r.el)
        case "/" => IntValue(l.el / r.el)
        case ">" =>  BoolValue(l.el >  r.el)
        case "<" =>  BoolValue(l.el <  r.el)
        case "<=" => BoolValue(l.el <= r.el)
        case ">=" => BoolValue(l.el >= r.el)
        case "==" => BoolValue(l.el == r.el)
      }
    }

    def intersectMaps[S, T](map1: Map[S, T], map2: Map[S, T]) = {
        val intersection: Map[S, T] = Map()
        for ((k, v) <- map1) {
          if (map2.contains(k) && map2(k) == v) {
            intersection(k) = v
          }
        }
        intersection
    }

    a match {
      case e @ Prog(args, bodypart) => Prog(args, visit(bodypart).asInstanceOf[Expression])
      case e: Body => Body(e.children.map(visit).asInstanceOf[Seq[Expression]])
      case Assignment(name, expr) => {
        val visitedExpr = visit(expr)
        visitedExpr match {
          case e: IntValue => {constantValues(name) = Right(e); return Assignment(name, e)}
          case e: BoolValue => {constantValues(name) = Left(e); return Assignment(name, e)}
          case e: Expression => {
            // The right-hand side of the assignment is not a constant

            // Remove the variable from our constants (if it was in there) since we can't constant fold it anymore.
            constantValues -= name;
            Assignment(name, visitedExpr.asInstanceOf[Expression])
          }
          case _ => throw new IllegalStateException("Tried to visit expression but got something else")
        }
      }
      case ValAssignment(name, expr) => {
        val visitedExpr = visit(expr)
        visitedExpr match {
          case e: IntValue => {constantValues(name) = Right(e); return ValAssignment(name, e)}
          case e: BoolValue => {constantValues(name) = Left(e); return ValAssignment(name, e)}
          case e: Expression => ValAssignment(name, visitedExpr.asInstanceOf[Expression])
          case _ => throw new IllegalStateException("Tried to visit expression but got something else")
        }
      }
      case VarAssignment(name, expr) => {
        val visitedExpr = visit(expr)
        visitedExpr match {
          case e: IntValue => {constantValues(name) = Right(e); VarAssignment(name, e)}
          case e: BoolValue => {constantValues(name) = Left(e); VarAssignment(name, e)}
          case e: Expression => {
            // The right-hand side of the assignment is not a constant

            // Remove the variable from our constants (if it was in there) since we can't constant fold it anymore.
            constantValues -= name;
            VarAssignment(name, visitedExpr.asInstanceOf[Expression])
          }
          case _ => throw new IllegalStateException("Tried to visit expression but got something else")
        }
      }
      case BinOp(left, right, op) => {
        val leftVisit = visit(left)
        val rightVisit = visit(right)
        leftVisit match {
          case l: IntValue => rightVisit match {
            case r: IntValue => return evalInt(l, r, op)
            case any => any
          }
          case l: BoolValue => rightVisit match {
            case r: BoolValue => return evalBool(l, r, op)
            case any => any
          }
          case any => any
        }
        BinOp(leftVisit.asInstanceOf[Expression], rightVisit.asInstanceOf[Expression], op)
      }
      case Branch(cond, body, elsebody) => {
        val condVisit = visit(cond)
        // FIXME: After the body visit, the constantValues have to be RESET for the elseBody visit
        val bodyVisit = visit(body)
        val constantsInIfBlock = constantValues.clone
        val elseVisit = visit(elsebody)
        val constantsInElseBlock = constantValues.clone
        constantValues = intersectMaps(constantsInIfBlock, constantsInElseBlock)

        Branch(condVisit.asInstanceOf[Expression], bodyVisit.asInstanceOf[Expression], elseVisit.asInstanceOf[Expression])
      }
      case x: BoolValue => x.cloneAST
      case x: IntValue => x.cloneAST
      case x: Deref => {
        if (constantValues contains x.name) {
          constantValues(x.name).merge
        } else {
          x
        }
      }
      case Coverage(body, covered) => Coverage(visit(body).asInstanceOf[Body], covered)
    }
  }
}

sealed class EMIModifier(rand: scala.util.Random) extends Visitor[AST, AST] {
  override def visit(a: AST): AST = {
    a match {
      case e @ Prog(args, bodypart) => Prog(args, visit(bodypart).asInstanceOf[Expression])
      case e @ Coverage(body, covered) => {
        if (!e.covered) {
          // decide whether we want to prune or not
          val nonPrunedExpressions: ListBuffer[Expression] = ListBuffer()
          val amountOfExpressions: Float = body.children.size
          for (expression <- body.children) {
            if (rand.nextFloat > 1.0 / (amountOfExpressions + 1)) {
              nonPrunedExpressions += expression
            }
          }
          Coverage(Body(nonPrunedExpressions.map(visit(_)).asInstanceOf[Seq[Expression]]), covered)
        } else {
          Coverage(visit(body).asInstanceOf[Body], covered)
        }
      }
      case e: Body => Body(e.children.map(visit).asInstanceOf[Seq[Expression]])
      case Assignment(name, expr) => Assignment(name, visit(expr).asInstanceOf[Expression])
      case ValAssignment(name, expr) => ValAssignment(name, visit(expr).asInstanceOf[Expression])
      case VarAssignment(name, expr) => VarAssignment(name, visit(expr).asInstanceOf[Expression])
      case BinOp(left, right, op) => BinOp(visit(left).asInstanceOf[Expression], visit(right).asInstanceOf[Expression], op)
      case Branch(cond, body, elsebody) => Branch(visit(cond).asInstanceOf[Expression], visit(body).asInstanceOf[Expression], visit(elsebody).asInstanceOf[Expression])
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
      case Branch(cond, body, elsebody) => Branch(visit(cond).asInstanceOf[Expression], visit(body).asInstanceOf[Expression], visit(elsebody).asInstanceOf[Expression])
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
      case e: BinOp => {
        val left = visit(e.left)
        val right = visit(e.right)
        val opSym = e.opSym
        if ((opSym == "&&" || opSym == "||") && (left == "Boolean" && right == "Boolean")) {
          "Boolean"
        } else if ((opSym == "<" || opSym == ">" || opSym == ">=" || opSym == "<=" || opSym == "==") && (left == "Int" && right == "Int")) {
          "Boolean"
        } else if ((opSym == "*" || opSym == "/" || opSym == "+" || opSym == "-") && (left == "Int" && right == "Int")) {
          "Int"
        } else {
          throw new SemanticCheckerException("Operator is '" + opSym + "'. Left expression has type " + left + " but right expression has type " + right)
        }
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
      case Coverage(b, covered) => visit(b)
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
        val result = visit(Prog(missingAssignments, body))
        result
      }
      case _ => throw new IllegalStateException("Can only simulate on the full Prog tree")
    }
  }

  override def visit(a: AST): Value = {
    a match {
      case e: Prog => visit(e.children, Left(false): Either[Boolean, Int])
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
      case e: BinOp => binOp(visit(e.left), visit(e.right), e.opSym)
      case e: Body => visit(e.children, Left(false): Either[Boolean, Int])
      case e: Branch =>
        if (visit(e.cond).left.get) {
          visit(e.body)
        } else {
          visit(e.elseBranch)
        }
    }
  }

  def binOp(e1: Value, e2: Value, opSym: String) = {
    opSym match {
      case "+" => Right(e1.right.get + e2.right.get)
      case "-" => Right(e1.right.get - e2.right.get)
      case "*" => Right(e1.right.get * e2.right.get)
      case "/" => Right(e1.right.get / e2.right.get)
      case ">" =>  Left(e1.right.get > e2.right.get)
      case "<" =>  Left(e1.right.get < e2.right.get)
      case "<=" => Left(e1.right.get <= e2.right.get)
      case ">=" => Left(e1.right.get >= e2.right.get)
      case "==" => Left(e1.right.get == e2.right.get)
      case "||" => Left(e1.left.get || e2.left.get)
      case "&&" => Left(e1.left.get && e2.left.get)
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

  def getIndent = "  " * indent

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
        val str = "{\n" + getIndent + "DEAD\n" + getIndent + visit(e.body) + "\n"
        indent -= 1
        str + getIndent + "}"
      }
    }
  }
}
