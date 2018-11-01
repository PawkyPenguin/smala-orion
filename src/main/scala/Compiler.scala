import scala.collection.JavaConversions._
import ASTTypes._
import Visitors._

object Compiler {
  //val optimizer: Optimizer = ???
  def compile(fromString: String): AST = {
    ScalaliParser.buildAST(fromString.trim)
  }
}

object ScalaliParser extends SimpleGrammarBaseVisitor[AST] {
  import org.antlr.v4.runtime._;
  import SimpleGrammarParser._

  def buildAST[T](code: String): AST = {
    val charStream = new ANTLRInputStream(code)
    val lexer = new SimpleGrammarLexer(charStream)
    val tokens = new CommonTokenStream(lexer)
    val parser = new SimpleGrammarParser(tokens)
    val tree = parser.eval()
    ScalaliParser.visit(tree)
  }

  override def visitEval(ctx: EvalContext): AST = {
    visit(ctx.children(0))
  }

  override def visitProg(ctx: ProgContext): AST = {
    Prog(ctx.argument.map(visit).asInstanceOf[Seq[ValAssignment]], visit(ctx.body).asInstanceOf[Body])
  }

  override def visitBody(ctx: SimpleGrammarParser.BodyContext): Body = {
    Body(ctx.line.toList.map {e => visit(e).asInstanceOf[Expression]})
  }

  override def visitAssignment(ctx: SimpleGrammarParser.AssignmentContext): Assignment = {
    Assignment(ctx.ident.getText, visit(ctx.expression).asInstanceOf[Expression])
  }

  override def visitVarAssignment(ctx: SimpleGrammarParser.VarAssignmentContext): VarAssignment = {
    VarAssignment(ctx.ident.getText, visit(ctx.expression).asInstanceOf[Expression])
  }

  override def visitValAssignment(ctx: SimpleGrammarParser.ValAssignmentContext): ValAssignment = {
    ValAssignment(ctx.ident.getText, visit(ctx.expression).asInstanceOf[Expression])
  }

  override def visitExpressionLine(ctx: SimpleGrammarParser.ExpressionLineContext): Expression = {
    visit(ctx.expression).asInstanceOf[Expression]
  }

  override def visitBoolexpr(ctx: SimpleGrammarParser.BoolexprContext): Expression = {
    val exp1 = visit(ctx.boolexpr1).asInstanceOf[Expression]
    if (ctx.boolexpr2 != null) {
      BinOp(exp1, visit(ctx.boolexpr2).asInstanceOf[Expression], ctx.boolexprOp.getText)
    } else {
      exp1
    }
  }

  override def visitArgument(ctx: SimpleGrammarParser.ArgumentContext): ValAssignment = {
    val value = if (ctx.case1 != null) {
      IntValue(ctx.case1.getText.toInt)
    } else {
      BoolValue(ctx.case2.getText.toBoolean)
    }
    ValAssignment(ctx.name.getText, value)
  }

  override def visitIfExpr(ctx: SimpleGrammarParser.IfExprContext): Expression = {
    if (ctx.elseBody != null) {
      Branch(visit(ctx.ifCond).asInstanceOf[Expression], visit(ctx.ifBody).asInstanceOf[Body], visit(ctx.elseBody).asInstanceOf[Body])
    } else {
      Branch(visit(ctx.ifCond).asInstanceOf[Expression], visit(ctx.ifBody).asInstanceOf[Body], Body(Seq.empty))
    }
  }

  override def visitMathexpr(ctx: SimpleGrammarParser.MathexprContext): Expression = {
    val exp1 = visit(ctx.mathexpr1).asInstanceOf[Expression]
    if (ctx.mathexpr2 != null) {
      BinOp(exp1, visit(ctx.mathexpr2).asInstanceOf[Expression], ctx.mathexprOp.getText())
    } else {
      exp1
    }
  }

  override def visitTerm(ctx: SimpleGrammarParser.TermContext): Expression = {
    val exp1 = visit(ctx.term1).asInstanceOf[Expression]
    if (ctx.term2 != null) {
      BinOp(exp1, visit(ctx.term2).asInstanceOf[Expression], ctx.termOp.getText())
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

  override def visitVariable(ctx: SimpleGrammarParser.VariableContext): Deref = {
    Deref(ctx.children(0).getText)
  }
}

package ASTTypes {
  sealed trait AST {
    def cloneAST: AST
    def children: Seq[AST]

    def visit[T](v: Visitor[AST, T]): T = {
      v.visit(this)
    }
  }

  final case class Coverage(body: Body, var covered: Boolean = false) extends Expression {
    def cloneAST = copy()
    def children = List(body)
  }

  final case class BinOp(left: Expression, right: Expression, opSym: String) extends Expression {
    def cloneAST = copy()
    def children = List(left, right)
  }

  final case class Assignment(variableName: String, expr: Expression) extends Expression {
    def cloneAST = copy()
    def children = List(expr)
  }

  final case class ValAssignment(variableName: String, expr: Expression) extends Expression {
    def cloneAST = copy()
    def children = List(expr)
  }

  final case class VarAssignment(variableName: String, expr: Expression) extends Expression {
    def cloneAST = copy()
    def children = List(expr)
  }

  final case class Branch(cond: Expression, body: Expression, elseBranch: Expression) extends Expression {
    def cloneAST = copy()
    def children = List(cond, body, elseBranch)
  }

  final case class Prog(args: Seq[ValAssignment], body: Expression) extends AST {
    def cloneAST = copy()
    def children = args :+ body
  }

  sealed case class Body(children: Seq[Expression]) extends Expression {
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

  final case class Deref(name: String) extends Expression {
    def cloneAST = copy()
    def children = Seq.empty
  }

  sealed trait Expression extends AST
}
