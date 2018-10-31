import scala.io.Source
import scala.collection.JavaConversions._
import ASTTypes._
import Visitors._

object Compiler {
  //val optimizer: Optimizer = ???

  def compile(filename: String, withCoverageProfiling: Boolean): AST = {
    val file = Source.fromFile(filename)
    val ast = MyVisitor.buildAST(file.mkString.trim)
    file.close
    ast
  }

}

object MyVisitor extends SimpleGrammarBaseVisitor[AST] {
  import org.antlr.v4.runtime._;
  import SimpleGrammarParser._

  def buildAST[T](code: String): AST = {
    val charStream = new ANTLRInputStream(code)
    val lexer = new SimpleGrammarLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new SimpleGrammarParser(tokens)
    val tree = parser.eval()
    MyVisitor.visit(tree)
  }

  override def visitEval(ctx: EvalContext): AST = {
    visit(ctx.children(0))
  }

  override def visitProg(ctx: ProgContext): AST = {
    visit(ctx.body)
  }

  override def visitBody(ctx: SimpleGrammarParser.BodyContext): Body = {
    Body(ctx.line.toList.map {e => visit(e).asInstanceOf[Expression]})
  }

  override def visitAssignment(ctx: SimpleGrammarParser.AssignmentContext): Assignment = {
    Assignment(ctx.ident.getText, visit(ctx.expression).asInstanceOf[Expression])
  }

  override def visitExpressionLine(ctx: SimpleGrammarParser.ExpressionLineContext): Expression = {
    visit(ctx.expression).asInstanceOf[Expression]
  }

  override def visitBoolexpr(ctx: SimpleGrammarParser.BoolexprContext): Expression = {
    val exp1 = visit(ctx.boolexpr1).asInstanceOf[Expression]
    if (ctx.boolexpr2 != null) {
      Condition(exp1, visit(ctx.boolexpr2).asInstanceOf[Expression], ctx.boolexprOp.getText)
    } else {
      exp1
    }
  }

  override def visitIfExpr(ctx: SimpleGrammarParser.IfExprContext): Expression = {
    Branch(visit(ctx.ifCond).asInstanceOf[Condition], visit(ctx.ifBody).asInstanceOf[Body], visit(ctx.elseBody).asInstanceOf[Body])
  }

  override def visitMathexpr(ctx: SimpleGrammarParser.MathexprContext): Expression = {
    val exp1 = visit(ctx.mathexpr1).asInstanceOf[Expression]
    if (ctx.mathexpr2 != null) {
      BinOp(exp1, visit(ctx.mathexpr2).asInstanceOf[Expression], ctx.mathexprOp.getText()(0))
    } else {
      exp1
    }
  }

  override def visitTerm(ctx: SimpleGrammarParser.TermContext): Expression = {
    val exp1 = visit(ctx.term1).asInstanceOf[Expression]
    if (ctx.term2 != null) {
      BinOp(exp1, visit(ctx.term2).asInstanceOf[Expression], ctx.termOp.getText()(0))
    } else {
      exp1
    }
  }

  override def visitNumberNs(ctx: SimpleGrammarParser.NumberNsContext): IntValue = {
    IntValue(ctx.getText().toInt)
  }

  override def visitBooleanNs(ctx: SimpleGrammarParser.BooleanNsContext): BoolValue = {
    BoolValue(ctx.getText().toBoolean)
  }

  override def visitBracketNs(ctx: SimpleGrammarParser.BracketNsContext): Expression = {
    visit(ctx.expression).asInstanceOf[Expression]
  }

  override def visitValue(ctx: SimpleGrammarParser.ValueContext): Variable = {
    Variable(ctx.children(0).getText)
  }
}

package ASTTypes {
  sealed trait CoverageAST {
    def visit[T](v: Visitor[CoverageAST, T]): T = {
      v.visit(this)
    }
  }

  final case class CoveragedBody(children: Seq[CoverageAST]) extends CoverageAST {
    var covered = false
  }

  sealed trait AST extends CoverageAST {
    def cloneAST: AST
    def children: Seq[AST]
  }

  final case class Empty() extends Expression {
    def cloneAST = copy()
    def children = Seq.empty
  }

  final case class BinOp(left: Expression, right: Expression, opSym: Char) extends Expression {
    def cloneAST = copy()
    def children = List(left, right)
  }

  final case class Assignment(variableName: String, expr: Expression) extends Expression {
    def cloneAST = copy()
    def children = List(expr)
  }

  final case class Branch(cond: Condition, body: Body, elseBranch: Body) extends Expression {
    def cloneAST = copy()
    def children = List(cond, body, elseBranch)
  }

  final case class Condition(left: Expression, right: Expression, opSym: String) extends Expression {
    def cloneAST = copy()
    def children = List(left, right)
  }

  final case class Body(children: Seq[Expression]) extends Expression {
    def cloneAST = copy()
  }

  sealed trait Value[T] extends Expression {
    def children = Seq.empty
  }

  final case class BoolValue(el: Boolean) extends Value[Boolean] {
    def cloneAST = copy()
  }

  final case class IntValue(el: Int) extends Value[Int] {
    def cloneAST = copy()
  }

  final case class Variable(name: String) extends Expression {
    def cloneAST = copy()
    def children = Seq.empty
  }

  sealed trait Expression extends AST
}
