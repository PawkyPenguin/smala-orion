import Visitors._
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import ASTTypes._
import scala.io.Source
import scala.util.Random
import java.io.PrintWriter
import java.nio.file.{FileSystems, Files, Paths, StandardOpenOption}
import scala.collection.JavaConverters._

object Main {
  val ITERATIONS = 200
  val INPUT_SET_SIZE = 2
  val GENERATED_TESTS = 20
  val simulator = new Simulator()
  val flattener = new Flattener()
  val coverager = new CoverageVisitor()
  val DEBUG = false

  implicit class MappableAST[To](a: AST) {
    def map(fun: AST => To) = {
      fun(a)
    }

    def optimize = a.map(new ConstantFolder().visit(_))

    def print: String = flattener.visit(a) + "\n"
  }

  def main(args: Array[String]): Unit = {
    val dir = FileSystems.getDefault.getPath("unittests/")
    val rand = new Random()
    rand.setSeed(System.currentTimeMillis())
    for (i <- 1 to ITERATIONS) {
      Files.newDirectoryStream(dir, "*.smala").iterator().asScala.filter(Files.isRegularFile(_)).foreach { f =>
        orion(f.getFileName.toString, INPUT_SET_SIZE, GENERATED_TESTS, rand)
        println("DONE ITERATION " + i + " =============================================================================================")
      }
    }
  }

  def orion(filename: String, generatedInputAmount: Int, iterations: Int, rand: Random): Unit = {
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
        val simulationResult = simulator.simulate(arguments)(astWithCoverage)
        results += simulationResult.merge
        i += 1
      }
      (inputs,results, astWithCoverage)
    }

    def compareIO(originalAst: AST, bugfile: String)(input: Map[String, Either[Boolean, Int]], refOut: AnyVal, modifiedAst: AST, testNum: Int) {
      val out: AnyVal = simulator.simulate(input)(modifiedAst).merge
      val inputStr = input.map{el => el._1 + "=" + el._2.merge}.mkString(", ")
      debug("Test " + testNum + ": " + inputStr + "\t->\t" + "(" + refOut + ", " + out + ") \t in file <unittests/" + bugfile + ">")
      if (out != refOut) {
        debug("MISTMATCH, FILING BUGREPORT")
        reportBug(input, refOut, out, originalAst, modifiedAst, bugfile + "_bugreport_" + System.currentTimeMillis() + ".txt")
      }
    }

    def reportBug(input: Map[String, Either[Boolean, Int]], refOut: AnyVal, actualOutput: AnyVal, originalAst: AST, modifiedAST: AST, filename: String) = {
      val reportPath = "unittests/bugreports/" + filename
      val marker = "========================================================================================\n"
      val originalAstPlusCoverage = originalAst.map(coverager.visit)
      simulator.simulate(input)(originalAstPlusCoverage)
      val bugreport = "Input = " + input + "\n" +
        "Reference output = " + refOut + "\n" +
        "Actual output = " + actualOutput + "\n" + marker +
        "Original AST = " +
        originalAst.print + marker +
        "AST with Coverage = " +
        originalAstPlusCoverage.print + marker +
        "Modified AST = " +
        modifiedAST.print
      val filepath = Paths.get(reportPath)
      Files.write(filepath, bugreport.getBytes("utf-8"), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    val randomPruneVisitor = new EMIModifier(rand)
    val filenameAndDir = "unittests/" + filename
    val unittest = Source.fromFile(filenameAndDir)
    val ast = SmalaCompiler.compile(unittest.mkString)
    println("Running Orion for File <" + filenameAndDir + ">")
    debug("Original unittest = " + ast.print)

    doSemanticCheck(ast)
    debug("Passed the semantic checks")
    val compareFunc = compareIO(ast, filename) _

    val (inputs, results, astWithCoverage) = runOnRandomInputs(ast, rand)
    debug("Coverage info = " + astWithCoverage.print)

    val optimizedAst = ast.optimize
    debug("Optimization done. Optimized AST = " + optimizedAst.print)

    val io = inputs.zip(results)
    // Generate random unit tests
    for ((input, refOut) <- io) {
      var i = 0
      val inputStr = "(" + input.map{el => el._1 + " -> " + el._2.merge}.mkString(", ") + ")"
      println("Doing tests for input = " + inputStr)
      compareFunc(input, refOut, optimizedAst, -1)
      while (i < iterations) {
        val generatedAst: AST = astWithCoverage.map(randomPruneVisitor.visit)
        val generatedAndOptimizedAst: AST = generatedAst.optimize
        compareFunc(input, refOut, generatedAst, i)
        compareFunc(input, refOut, generatedAndOptimizedAst, i)
        i += 1
      }
      debug("")
    }
    unittest.close
  }

  def debug(str: String) = if (DEBUG) println(str)


}

final case class OrionException(msg: String) extends Exception(msg)

object MyLeft {
  def apply(a: Boolean): Either[Boolean, Int] = Left(a)
}

object MyRight {
  def apply(a: Int): Either[Boolean, Int] = Right(a)
}
