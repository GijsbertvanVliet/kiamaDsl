package gb.kiama.dsl

import gb.kiama.dsl.MyTree._
import org.bitbucket.inkytonik.kiama.parsing.{ParseResult, Success}
import org.bitbucket.inkytonik.kiama.util.Positions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFlatSpec with Matchers {

  /**
   * root(input: Int, secondInput: String) {
   *    val a: String = "something"
   *    val b: Int = 1 + -2
   *    val c: String = a
   *    val d: Int = secondInput;
   *    return c + "hello" + input
   * }
   */

  private val twoLiteral              = IntegerLiteralExpression(2)
  private val unaryExpression         = NegateExpression(twoLiteral)
  private val oneLiteral              = IntegerLiteralExpression(1)
  private val integerBinaryExpression = AddExpression(oneLiteral, unaryExpression)
  private val somethingLiteral        = StringLiteralExpression("something")
  private val variableAssignmentA     = VariableAssignmentStatement(StringType, "a", somethingLiteral)
  private val variableAssignmentB     = VariableAssignmentStatement(IntegerType, "b", integerBinaryExpression)
  private val localVarRefToA          = LocalVariableReferenceExpression("a")
  private val variableAssignmentC     = VariableAssignmentStatement(StringType, "c", localVarRefToA)
  private val inputArgument1          = InputArgument(IntegerType, "input")
  private val inputArgument2          = InputArgument(StringType, "secondInput")
  private val localVarRefToC          = LocalVariableReferenceExpression("c")
  private val cPlusHello              = AddExpression(localVarRefToC, StringLiteralExpression("hello"))
  private val localVarRefToInput      = LocalVariableReferenceExpression("input")
  private val stringBinaryExpression  = AddExpression(cPlusHello, localVarRefToInput)
  private val secondInputReference    = LocalVariableReferenceExpression("secondInput")
  val variableAssignmentD             = VariableAssignmentStatement(IntegerType, "d", secondInputReference)
  private val returnStatement         = Return(stringBinaryExpression)
  private val root =
    Root(Seq(inputArgument1, inputArgument2), Seq(variableAssignmentA, variableAssignmentB, variableAssignmentC, variableAssignmentD), returnStatement)
  private val tree = new MyTree(root)

  val treeManipulations = new MyTreeManipulations(tree)
  import treeManipulations._

  it should "parse text to tree object" in {
    val positions = new Positions
    val result: ParseResult[Root] = new SyntaxAnalyser(positions).rootParser(
      """root(input: Int, secondInput: String) {
        |  val a: String = "something";
        |  val b: Int = 1 + -2;
        |  val c: String = a;
        |  val d: Int = secondInput;
        |  return c + "hello" + input;
        |}
        |""".stripMargin
    )

    result match {
      case Success(result, _) => result shouldBe root
      case _                  => true shouldBe false
    }
  }

  it should "get all parents" in {
    getParents(unaryExpression) should contain theSameElementsAs Seq(integerBinaryExpression, variableAssignmentB, root)
    getParents(localVarRefToA) should contain theSameElementsAs Seq(variableAssignmentC, root)
  }

  it should "get all left siblings" in {
    getLeftSiblings(variableAssignmentC) should contain theSameElementsAs Seq(inputArgument1, inputArgument2, variableAssignmentA, variableAssignmentB)
    getLeftSiblings(variableAssignmentB) should contain theSameElementsAs Seq(inputArgument1, inputArgument2, variableAssignmentA)
    getLeftSiblings(variableAssignmentA) should contain theSameElementsAs Seq(inputArgument1, inputArgument2)
    getLeftSiblings(inputArgument2) should contain theSameElementsAs Seq(inputArgument1)
    getLeftSiblings(inputArgument1) shouldBe empty

    getLeftSiblings(unaryExpression) should contain theSameElementsAs Seq(oneLiteral)
    getLeftSiblings(oneLiteral) shouldBe empty
  }

  it should "get all right siblings" in {
    getRightSiblings(variableAssignmentA) should contain theSameElementsAs Seq(variableAssignmentB, variableAssignmentC, variableAssignmentD, returnStatement)
    getRightSiblings(variableAssignmentB) should contain theSameElementsAs Seq(variableAssignmentC, variableAssignmentD, returnStatement)
    getRightSiblings(variableAssignmentC) should contain theSameElementsAs Seq(variableAssignmentD, returnStatement)
    getRightSiblings(returnStatement) shouldBe empty

    getRightSiblings(oneLiteral) should contain theSameElementsAs Seq(unaryExpression)
    getRightSiblings(unaryExpression) shouldBe empty
  }

  it should "get all binary expressions" in {
    val binaryExpressions = getNodes[BinaryExpression]
    binaryExpressions should contain theSameElementsAs Seq(integerBinaryExpression, stringBinaryExpression, cPlusHello)
  }

  it should "get localVariables in scope" in {
    getAllLocalVariablesInScope(unaryExpression) should contain theSameElementsAs Seq(variableAssignmentA, inputArgument1, inputArgument2)
    getAllLocalVariablesInScope(somethingLiteral) should contain theSameElementsAs Seq(inputArgument1, inputArgument2)
    getAllLocalVariablesInScope(localVarRefToA) should contain theSameElementsAs Seq(variableAssignmentA, variableAssignmentB, inputArgument1, inputArgument2)

    getReferencedVariableAssignment(localVarRefToA) shouldBe Option(variableAssignmentA.exp)
  }

  it should "optimize all variables" in {
    val resolvedLocalVars: Root = resolveLocalVariables
    val newTree: MyTree         = new MyTree(resolvedLocalVars)
    val reduced: Root           = new MyTreeManipulations(newTree).resolveUnariesAndBinaries
    val layout                  = PrettyPrinter.format(reduced).layout

    val expected =
      """root (input: Int, secondInput: String) {
        |    val a: String = "something";
        |    val b: Int = -1;
        |    val c: String = "something";
        |    val d: Int = secondInput;
        |    return "somethinghello" + input;
        |}""".stripMargin

    layout shouldBe expected
  }

  it should "validate expression return types or return validation errors" in {
    val result = checkExpressionReturnTypesOrGetValidationMessages
    result.messages should contain theSameElementsAs Seq(
      "Expression returns a StringType, but local variable assignment actually expects a IntegerType",
      "Cannot add a StringType to a IntegerType"
    )
  }

}
