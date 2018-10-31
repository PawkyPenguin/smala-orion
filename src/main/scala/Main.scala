import Visitors._

object Main {
  def main(args: Array[String]) = {
    val ast = Compiler.compile("test", false)
    val simulator = new Simulator()
    val flattener = new Flattener()
    val coveragedAst = new Coverage().visit(ast)
    println("print flattened  AST: " + flattener.visit(ast))
    println("simulation says     : " + simulator.visit(coveragedAst).merge)
  }
}
