package gb.kiama.dsl

import gb.kiama.dsl.MyTree._
import org.bitbucket.inkytonik.kiama.parsing.Success
import org.bitbucket.inkytonik.kiama.util.Positions
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Tests extends AnyFlatSpec with Matchers {

  private val exampleDsl =
    """root(input: Int, secondInput: String) {
      |  val a: String = "something";
      |  val b: Int = 1 + -2;
      |  val c: String = a;
      |  val d: Int = secondInput;
      |  return c + "hello" + input;
      |}
      |""".stripMargin

  val positions = new Positions

  private val root: Root =
    new SyntaxAnalyser(positions).rootParser(exampleDsl) match {
      case Success(result, _) => result
      case _                  => throw new Exception("given dsl should be parseable")
    }

  private val tree = new MyTree(root)

  private val treeManipulations = new MyTreeManipulations(tree)

  private val returnStatement         = root.returnStatement
  private val variableAssignmentA     = root.statements.head.asInstanceOf[VariableAssignmentStatement]
  private val variableAssignmentB     = root.statements(1).asInstanceOf[VariableAssignmentStatement]
  private val variableAssignmentC     = root.statements(2).asInstanceOf[VariableAssignmentStatement]
  private val variableAssignmentD     = root.statements(3).asInstanceOf[VariableAssignmentStatement]
  private val integerBinaryExpression = variableAssignmentB.exp.asInstanceOf[AddExpression]
  private val unaryExpression         = integerBinaryExpression.right.asInstanceOf[NegateExpression]
  private val inputArgument1          = root.inputArguments.head
  private val inputArgument2          = root.inputArguments(1)
  private val localVarRefToA          = variableAssignmentC.exp.asInstanceOf[LocalVariableReferenceExpression]
  private val oneLiteral              = integerBinaryExpression.left.asInstanceOf[IntegerLiteralExpression]
  private val stringBinaryExpression  = returnStatement.expression.asInstanceOf[AddExpression]
  private val cPlusHello              = stringBinaryExpression.left.asInstanceOf[AddExpression]
  private val somethingLiteral        = variableAssignmentA.exp.asInstanceOf[StringLiteralExpression]
  private val localVarRefToInput      = stringBinaryExpression.right.asInstanceOf[LocalVariableReferenceExpression]
  private val localVarRefToC          = cPlusHello.left.asInstanceOf[LocalVariableReferenceExpression]

  private def runTestOnTree(unitTest: (Positions, MyTreeManipulations) => Any)(inputDsl: String = exampleDsl) = {
    val positions = new Positions

    new SyntaxAnalyser(positions).rootParser(inputDsl) match {
      case Success(result, _) =>
        val tree          = new MyTree(result)
        val manipulations = new MyTreeManipulations(tree)
        unitTest(positions, manipulations)
      case _ => fail("failed to parse input")
    }
  }

  it should "get all parents" in {
    import treeManipulations._

    getParents(unaryExpression) should contain theSameElementsAs Seq(integerBinaryExpression, variableAssignmentB, root)
    getParents(localVarRefToA) should contain theSameElementsAs Seq(variableAssignmentC, root)
  }

  it should "get all left siblings" in {
    import treeManipulations._

    getLeftSiblings(variableAssignmentC) should contain theSameElementsAs Seq(inputArgument1, inputArgument2, variableAssignmentA, variableAssignmentB)
    getLeftSiblings(variableAssignmentB) should contain theSameElementsAs Seq(inputArgument1, inputArgument2, variableAssignmentA)
    getLeftSiblings(variableAssignmentA) should contain theSameElementsAs Seq(inputArgument1, inputArgument2)
    getLeftSiblings(inputArgument2) should contain theSameElementsAs Seq(inputArgument1)
    getLeftSiblings(inputArgument1) shouldBe empty

    getLeftSiblings(unaryExpression) should contain theSameElementsAs Seq(oneLiteral)
    getLeftSiblings(oneLiteral) shouldBe empty
  }

  it should "get all right siblings" in {
    import treeManipulations._

    getRightSiblings(variableAssignmentA) should contain theSameElementsAs Seq(variableAssignmentB, variableAssignmentC, variableAssignmentD, returnStatement)
    getRightSiblings(variableAssignmentB) should contain theSameElementsAs Seq(variableAssignmentC, variableAssignmentD, returnStatement)
    getRightSiblings(variableAssignmentC) should contain theSameElementsAs Seq(variableAssignmentD, returnStatement)
    getRightSiblings(returnStatement) shouldBe empty

    getRightSiblings(oneLiteral) should contain theSameElementsAs Seq(unaryExpression)
    getRightSiblings(unaryExpression) shouldBe empty
  }

  it should "get all binary expressions" in {
    import treeManipulations._

    val binaryExpressions = getNodes[BinaryExpression]()
    binaryExpressions should contain theSameElementsAs Seq(integerBinaryExpression, stringBinaryExpression, cPlusHello)
  }

  it should "get localVariables in scope" in {
    import treeManipulations._

    getAllLocalVariablesInScope(unaryExpression) should contain theSameElementsAs Seq(variableAssignmentA, inputArgument1, inputArgument2)
    getAllLocalVariablesInScope(somethingLiteral) should contain theSameElementsAs Seq(inputArgument1, inputArgument2)
    getAllLocalVariablesInScope(localVarRefToA) should contain theSameElementsAs Seq(variableAssignmentA, variableAssignmentB, inputArgument1, inputArgument2)

    getReferencedVariableAssignmentExpression(localVarRefToA) shouldBe Option(variableAssignmentA.exp)
  }

  it should "get local variable expressions pointing to assignments" in {
    import treeManipulations._

    getLocalVariableReferencesPointingToAssignment(inputArgument1) should contain theSameElementsAs Seq(localVarRefToInput)
    getLocalVariableReferencesPointingToAssignment(variableAssignmentC) should contain theSameElementsAs Seq(localVarRefToC)
  }

  it should "optimize all variables" in {
    import treeManipulations._

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
    import treeManipulations._

    val result = checkExpressionReturnTypesOrGetValidationMessages
    result.messages should contain theSameElementsAs Seq(
      "Expression returns a StringType, but local variable assignment actually expects a IntegerType",
      "Cannot add a StringType to a IntegerType"
    )
  }

  it should "find unused variable assignments" in {
    runTestOnTree { (_, manipulations) =>
      manipulations.getUnusedAssignments shouldBe ValidationMessages(Seq("assignment val b: Int = 1 + -2; is never used.", "assignment val d: Int = secondInput; is never used."))
    }()

  }

  it should "find faulty local variable references" in {
    runTestOnTree { (_, manipulations) =>
      manipulations.findLocalVarRefsWithoutOrigin shouldBe ValidationMessages("local variable reference 'input' does not point to any assignment")
    }(
      """root (input: Int) {
        |    val a: String = "something";
        |    return input;
        |}""".stripMargin
    )
  }

  it should "create validations for duplicate defined local variable assignments" in {
    runTestOnTree { (_, manipulations) =>
      manipulations.findDuplicateVariableAssignments shouldBe ValidationMessages("there are multiple assignments with name 'input' in scope")
    }(
      """root (input: Int) {
        |    val input: String = "something";
        |    return input;
        |}""".stripMargin
    )
  }

  it should "remove any unused statements" in {
    import treeManipulations._

    val treeWithRemovedUnusedVariables = removeUnusedStatements
    treeWithRemovedUnusedVariables.inputArguments should contain theSameElementsAs Seq(inputArgument1)
    treeWithRemovedUnusedVariables.statements should contain theSameElementsAs Seq(variableAssignmentA, variableAssignmentC)
  }
}
