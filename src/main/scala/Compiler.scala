import scala.io.Source
import scala.collection.JavaConversions._

object Compiler {
  //val optimizer: Optimizer = ???

  def compile(filename: String, withCoverageProfiling: Boolean): Unit = {
    val file = Source.fromFile(filename)
    val ast = MyVisitor.buildAST(file.getLines().toString)
    file.close
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
    //super.visit(ctx)
    ???
  }

  override def visitProg(ctx: ProgContext): AST = {
    print(ctx.children)
    val a = ctx.children
    ???
  }

  override def visitBody(ctx: SimpleGrammarParser.BodyContext): Body = {
    //Body(ctx.children.map(visit))
    ???
  }

  override def visitAssignment(ctx: SimpleGrammarParser.AssignmentContext): Assignment = {
    ???
  }

  override def visitExpressionLine(ctx: SimpleGrammarParser.ExpressionLineContext): Expression = {
    ???
  }

  override def visitBoolexpr(ctx: SimpleGrammarParser.BoolexprContext): AST = {
    ???
  }

  override def visitIfExpr(ctx: SimpleGrammarParser.IfExprContext): AST = {
    ???
  }

  override def visitMathexpr(ctx: SimpleGrammarParser.MathexprContext): Expression = {
    ???
  }

  override def visitTerm(ctx: SimpleGrammarParser.TermContext): Expression = {
    ???
  }

  override def visitFactor(ctx: SimpleGrammarParser.FactorContext): Expression = {
    ???
  }

  override def visitNumberNs(ctx: SimpleGrammarParser.NumberNsContext): IntValue = {
    ???
  }

  override def visitBooleanNs(ctx: SimpleGrammarParser.BooleanNsContext): BoolValue = {
    ???
  }

  override def visitBracketNs(ctx: SimpleGrammarParser.BracketNsContext): Expression = {
    ???
  }

  override def visitValue(ctx: SimpleGrammarParser.ValueContext): Variable = {
    ???
  }
}

sealed trait AST {
  def children: Seq[AST]
  def visit[T](v: Visitor[T]): T = {
    v.visitThis(this)
  }
}

trait Ops
object Plus extends Ops
object Minus extends Ops
object Mult extends Ops
object Div extends Ops

object Empty extends Expression {
  def children = Seq.empty
}

final case class BinOp(left: Expression, right: Expression, op: Ops) extends Expression {

  def children = List(left, right)
}

object BinOp {
  def apply(left: Expression, right: Expression, opSym: Char): BinOp = {
    val op = opSym match {
      case '+' => Plus
      case '-' => Minus
      case '*' => Mult
      case '/' => Div
      case _ => throw new IllegalArgumentException
    }
    BinOp(left, right, op)
  }
}

final case class Assignment(variable: Variable, expr: Expression) extends Expression {
  def children = List(variable, expr)
}

final case class Branch(cond: Condition, body: Body, elseBranch: Body) extends Expression {
  def children = List(cond, body, elseBranch)
}

final case class Condition(cond: Expression) extends Expression {
  def children = List(cond)
}

final case class Body(children: Seq[Expression]) extends Expression {
}

sealed trait Value[T] extends Expression {
  def children = Seq.empty
}

final case class BoolValue(el: Boolean) extends Value[Boolean] {
}

final case class IntValue(el: Int) extends Value[Int] {
}

final case class Variable(name: String) extends Expression {
  def children = Seq.empty
}

sealed trait Expression extends AST {
}
