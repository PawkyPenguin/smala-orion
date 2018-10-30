import scala.io.Source

object Compiler {
  val optimizer: Optimizer = ???

  def compile(filename: String, withCoverageProfiling: Boolean): Unit = {
    val file = Source.fromFile(filename)
    val ast = Parser.buildAST(file)
    file.close
  }

}

object Parser {
  type Token = String

  def buildAST[T](file: Source): AST = {
    //val expressions = file.getLines().map { parseLine _ }
    //ExpressionSeq(expressions.toList)
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

final case class Branch(cond: Condition, body: Body, elseBranch: Body) extends Expression {
  def children = List(cond, body, elseBranch)
}

final case class Condition(cond: Expression) extends Expression {
  def children = List(cond)
}

final case class Body(children: Seq[Expression]) extends Expression {
}

final case class Value[T](el: T) extends Expression {
  def children = Seq.empty
}

final case class ExpressionSeq(children: Seq[Expression]) extends AST {
}

sealed trait Expression extends AST {
}
