package gb.kiama.dsl

import gb.kiama.dsl.MyTree._
import org.bitbucket.inkytonik.kiama.parsing.{NoSuccess, Success}
import org.bitbucket.inkytonik.kiama.util.Positions

object ApplicationExecuter {

  private type ExecutionContext           = Map[String, Any]
  private type ValidationMessage          = String
  private type ApplicationExecutionResult = (ValidationMessage, Option[Any])

  def executeApplication(dsl: String, inputVariables: Seq[Any]): ApplicationExecutionResult = {
    implicit val positions: Positions = new Positions

    new SyntaxAnalyser().rootParser(dsl) match {
      case Success(root, _) =>
        val tree              = new MyTree(root)
        val treeManipulations = new MyTreeManipulations(tree)
        val validations       = validateTree(treeManipulations)

        if (validations.containsError) validations.toString -> None
        else {
          val optimizedRoot                             = optimizeTree(treeManipulations)
          val (additionalValidations, evaluationResult) = evaluateOptimizedTree(optimizedRoot, inputVariables)
          (validations ++ additionalValidations).toString -> evaluationResult
        }

      case NoSuccess(message, _) => ValidationMessages(message, Error).toString -> None
    }
  }

  private def evaluateOptimizedTree(optimizedRoot: Root, inputVariables: Seq[Any])(implicit positions: Positions): (ValidationMessages, Option[Any]) = {
    val (additionalValidations, executionContext) = createExecutionContext(optimizedRoot.inputArguments, inputVariables)
    additionalValidations -> evaluateExpression(optimizedRoot.returnStatement.expression, executionContext)
  }

  private def createExecutionContext(inputArguments: Seq[InputArgument], inputVariables: Seq[Any])(implicit positions: Positions): (ValidationMessages, ExecutionContext) =
    if (inputArguments.size != inputVariables.size)
      ValidationMessages(s"Defined ${inputVariables.size} input arguments where ${inputArguments.size} were expected.", Error) -> Map.empty
    else {
      val result: Seq[(ValidationMessages, Option[(String, Any)])] = inputArguments.zip(inputVariables).map {
        case (InputArgument(StringType, variableName), value: String)   => ValidationMessages.empty                                                                                                           -> Some(variableName -> value)
        case (InputArgument(IntegerType, variableName), value: Integer) => ValidationMessages.empty                                                                                                           -> Some(variableName -> value)
        case (inputArgument, value)                                     => ValidationMessages(s"Expected a ${inputArgument.dataType} input argument, but a ${value.getClass.getSimpleName} was given", Error) -> None
      }

      ValidationMessages.concatenate(result.map(_._1)) -> result.flatMap(_._2).toMap
    }

  private def optimizeTree(treeManipulations: MyTreeManipulations)(implicit positions: Positions): Root = {
    val resolvedLocalVars: Root = treeManipulations.resolveLocalVariables

    val newTree: MyTree = new MyTree(resolvedLocalVars)
    new MyTreeManipulations(newTree).resolveUnariesAndBinaries
  }

  private def validateTree(treeManipulations: MyTreeManipulations)(implicit positions: Positions): ValidationMessages = {
    import treeManipulations._
    checkExpressionReturnTypesOrGetValidationMessages ++ getUnusedAssignments ++ findLocalVarRefsWithoutOrigin ++ findDuplicateVariableAssignments
  }

  private def evaluateExpression(expression: Expression, localScope: ExecutionContext): Option[Any] = expression match {
    case integerLiteral: IntegerLiteralExpression => Some(integerLiteral.value)
    case stringLiteral: StringLiteralExpression   => Some(stringLiteral.value)
    case AddExpression(left, right) =>
      (evaluateExpression(left, localScope), evaluateExpression(right, localScope)) match {
        case (Some(leftInt: Integer), Some(rightInt: Integer))     => Some(leftInt + rightInt)
        case (Some(leftString: String), Some(rightString: String)) => Some(leftString + rightString)
        case _                                                     => None
      }
    case SubExpression(left, right) =>
      (evaluateExpression(left, localScope), evaluateExpression(right, localScope)) match {
        case (Some(leftInt: Integer), Some(rightInt: Integer)) => Some(leftInt - rightInt)
        case _                                                 => None
      }
    case NegateExpression(exp) =>
      evaluateExpression(exp, localScope) match {
        case Some(int: Integer) => Some(-int)
        case _                  => None
      }
    case LocalVariableReferenceExpression(referenceName) => localScope.get(referenceName)
  }
}
