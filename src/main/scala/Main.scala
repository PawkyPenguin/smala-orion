import Visitors._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import ASTTypes._
import scala.io.Source
import scala.util.Random
import java.io.PrintWriter
import java.nio.file.{FileSystems, Files}
import scala.collection.JavaConverters._

final case class OrionException(msg: String) extends Exception(msg)

object MyLeft {
  def apply(a: Boolean): Either[Boolean, Int] = Left(a)
}

object MyRight {
  def apply(a: Int): Either[Boolean, Int] = Right(a)
}

object Main {
  val ITERATIONS = 10
  val INPUT_SET_SIZE = 2
  val simulator = new Simulator()
  val flattener = new Flattener()
  val coverager = new CoverageVisitor()

  implicit class MappableAST[To](a: AST) {
    def map(fun: AST => To) = {
      fun(a)
    }

    def print: String = flattener.visit(a) + "\n"
  }

  def main(args: Array[String]): Unit = {
    val dir = FileSystems.getDefault.getPath("unittests/")
    //Files.newDirectoryStream(dir, "*.txt").iterator().asScala.filter(Files.isRegularFile(_)).foreach { f =>
    //  orion(f.getFileName.toString, INPUT_SET_SIZE, ITERATIONS)
    //}
    orion("test2.txt", INPUT_SET_SIZE, ITERATIONS)
  }

  def orion(filename: String, generatedInputAmount: Int, iterations: Int): Unit = {
    def doSemanticCheck(ast: AST) = {
        ast.map(new SemanticChecker().visit)
    }

      def generateRandomValue(rand: Random, value: Expression) =
        value match {
          case v: IntValue => MyRight(rand.nextInt(200) - 100)
          case v: BoolValue => MyLeft(rand.nextBoolean)
          case _ => throw new IllegalStateException("Values for arguments are something other than Int or Boolean. Make sure the argument visitor method in the parser only creates IntValue or BoolValue")
        }

      def generateRandomInputs(rand: Random, coverage: Prog): Map[String, Either[Boolean, Int]] =
        coverage match {
          case Prog(args, _) => {
            val inputMap: Map[String, Either[Boolean, Int]] = Map()
            for (arg @ ValAssignment(name, value) <- args) {
              inputMap(name) = generateRandomValue(rand, value)
            }
            inputMap
          }
        }

      def runOnRandomInputs(ast: AST, rand: Random) = {
        var astWithCoverage = ast.map(coverager.visit)
        val inputs: ListBuffer[Map[String, Either[Boolean, Int]]] = ListBuffer()
        val results: ListBuffer[AnyVal] = ListBuffer()
        // Generate random inputs #generatedInputAmount times. This gets us coverage information.
        var i = 0
        while (i < generatedInputAmount) {
          val arguments = generateRandomInputs(rand, astWithCoverage.asInstanceOf[Prog])
          inputs += arguments
          val simulationResult = astWithCoverage.map(simulator.simulate(arguments))
          results += simulationResult.merge
          i += 1
        }
        (inputs,results, astWithCoverage)
      }

      def reportBug(ast: AST, optimizedAst: AST, astWithCoverage: AST, modifiedAST: AST, filename: String) = {
        val marker = "========================================================================================\n"
        new PrintWriter("unittest/bugreports/" + filename) { try {write(
          "Bug report...\n" +
          "Original AST = " +
          ast.print + marker +
          "Optimized ASt = " +
          optimizedAst.print + marker +
          "Ast + Coverage = " + 
          astWithCoverage.print + marker +
          "Modified AST = " +
          modifiedAST.print
        )} finally {close} }
      }

      val rand = Random
      val randomPruneVisitor = new EMIModifier(rand)
      val filenameAndDir = "unittests/" + filename
      val unittest = Source.fromFile(filenameAndDir)
      val ast = Compiler.compile(unittest.mkString)
      println("Running Orion for File <" + filenameAndDir + ">")
      println("Original unittest = " + ast.print)
      doSemanticCheck(ast)
      println("Passed the semantic checks")
      val optimizedAst = ast.map(new ConstantFolder().visit(_))
      println("Optimization done. New AST = " + optimizedAst.print)
      val (inputs, results, astWithCoverage) = runOnRandomInputs(optimizedAst, rand)
      println("Coverage info = " + astWithCoverage.print)

      var i = 0
      // Generate random unit tests
      while (i < iterations) {
        for ((input, originalOutput) <- inputs.zip(results)) {
          val modifiedAST: AST = astWithCoverage.map(randomPruneVisitor.visit)
          val output: AnyVal = modifiedAST.map(simulator.simulate(input)).merge
          val inputStr = input.map{el => el._1 + "=" + el._2.merge}.mkString(", ")
          println("Test " + i + ": " + inputStr + "\t->\t" + "(" + originalOutput + ", " + output + ") \t in file <" + filenameAndDir + ">")
          if (output != originalOutput) {
            println("MISTMATCH, FILING BUGREPORT")
            reportBug(ast, optimizedAst, astWithCoverage, modifiedAST, filename + "_bugreport_" + System.currentTimeMillis() + ".txt")
          }
        }
        i += 1
      }
      unittest.close
  }

}

//trait Monad[T[_]] {
//  def point[A](value: A): T[A]
//  def flatMap[A, B](m: T[A])(fun: A => T[B]): T[B]
//  def map[A, B](m: T[A])(fun: A => B): T[B] = flatMap(m)(fun andThen point)
//}
