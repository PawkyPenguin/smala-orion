sealed trait Visitor[T] {
  def visitThis(a: AST): T
}

sealed class Optimizer extends Visitor[AST]{
  override def visitThis(a: AST): AST = {
    ???
  }
}

sealed class Coverage extends Visitor[AST] {
  override def visitThis(a: AST): AST = {
    ???
  }
}

sealed class Flattener extends Visitor[Seq[String]] {
  override def visitThis(a: AST): Seq[String] = {
    ???
  }
}
