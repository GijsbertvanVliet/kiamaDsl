package gb.kiama.dsl

import gb.kiama.dsl.MyTree._
import org.bitbucket.inkytonik.kiama.attribution.UncachedAttribution.attr
import org.bitbucket.inkytonik.kiama.rewriting.{Rewriter, Strategy}

import scala.reflect.ClassTag

class MyTreeManipulations(tree: MyTree) extends Rewriter {
  val getParents: TreeNode => Seq[TreeNode] =
    attr {
      case tree.parent(p) => p +: getParents(p)
      case _              => Seq.empty
    }

  val getLeftSiblings: TreeNode => Seq[TreeNode] =
    attr {
      case tree.prev(left) => left +: getLeftSiblings(left)
      case _               => Seq.empty
    }

  val getRightSiblings: TreeNode => Seq[TreeNode] =
    attr {
      case tree.next(right) => right +: getRightSiblings(right)
      case _                => Seq.empty
    }

  def getNodes[T <: TreeNode: ClassTag](treeNode: TreeNode = tree.root): Seq[T] = collectl { case node: T =>
    node
  }(treeNode)

  val getAllLocalVariablesInScope: TreeNode => Seq[VariableAssignment] = attr { node: TreeNode =>
    val statementParents = (node +: getParents(node)).collect { case statement: Statement =>
      statement
    }

    statementParents.flatMap(statement =>
      getLeftSiblings(statement).collect { case assignment: VariableAssignment =>
        assignment
      }
    )
  }

  val getReferencedVariableAssignmentExpression: LocalVariableReferenceExpression => Option[Expression] = attr { node: LocalVariableReferenceExpression =>
    getReferencesVariableAssignment(node).collectFirst { case variableAssignment: VariableAssignmentStatement =>
      variableAssignment.exp
    }

  }

  val getReferencesVariableAssignment: LocalVariableReferenceExpression => Seq[VariableAssignment] = attr { node: LocalVariableReferenceExpression =>
    getAllLocalVariablesInScope(node).filter(_.variableName == node.referenceName).collect { case variableAssignment: VariableAssignment =>
      variableAssignment
    }
  }

  val getLocalVariableReferencesPointingToAssignment: VariableAssignment => Seq[LocalVariableReferenceExpression] = attr { node: VariableAssignment =>
    getRightSiblings(node).flatMap(sibling => getNodes[LocalVariableReferenceExpression](sibling).filter(_.referenceName == node.variableName))
  }

  val reduceBinaryAndUnaryExpressionStrategy: Strategy = rule[TreeNode] {
    case binaryExpression: BinaryExpression => binaryExpression.reduce
    case unaryExpression: UnaryExpression   => unaryExpression.reduce
  }

  val replaceLocalVariableReferencesStrategy: Strategy = strategy[TreeNode] { case localVarRef: LocalVariableReferenceExpression =>
    getReferencedVariableAssignmentExpression(localVarRef)
  }

  val resolveUnariesAndBinaries: Root = rewrite(bottomup(reduceBinaryAndUnaryExpressionStrategy <+ id))(tree.root)

  val resolveLocalVariables: Root = rewrite(reduce(replaceLocalVariableReferencesStrategy))(tree.root)

  def getUsedVariableAssignments(expression: Expression = tree.root.returnStatement.expression): Seq[VariableAssignment] =
    getNodes[LocalVariableReferenceExpression](expression).flatMap { localVarRef =>
      getReferencesVariableAssignment(localVarRef).collect {
        case assignmentStatement: VariableAssignmentStatement =>
          (assignmentStatement +: getUsedVariableAssignments(assignmentStatement.exp)).distinct
        case inputArgument: InputArgument => Seq(inputArgument)
      }.flatten

    }

  val removeUnusedStatements: Root = {
    val usedVariables = getUsedVariableAssignments()
    tree.root.copy(inputArguments = tree.root.inputArguments.filter(usedVariables.contains), statements = tree.root.statements.filter(usedVariables.contains))
  }

  //<editor-fold defaultstate="collapsed"  desc="validations">

  private def checkVariableAssignmentsDataType: ValidationMessages = ValidationMessages.concatenate(
    getNodes[VariableAssignmentStatement]()
      .map { variableAssignment =>
        variableAssignment.exp.getReturnType(getAllLocalVariablesInScope(variableAssignment)) match {
          case Left(validationMessages)                                   => validationMessages
          case Right(dataType) if dataType == variableAssignment.dataType => ValidationMessages.empty
          case Right(dataType)                                            => ValidationMessages(s"Expression returns a $dataType, but local variable assignment actually expects a ${variableAssignment.dataType}")
        }
      }
  )

  private def checkReturnExpressionDataType: ValidationMessages = {
    val returnExpression = tree.root.returnStatement.expression
    returnExpression
      .getReturnType(getAllLocalVariablesInScope(returnExpression))
      .left
      .getOrElse(ValidationMessages.empty)
  }

  def checkExpressionReturnTypesOrGetValidationMessages: ValidationMessages =
    checkVariableAssignmentsDataType ++ checkReturnExpressionDataType

  def getUnusedAssignments: ValidationMessages =
    ValidationMessages.concatenate(getNodes[VariableAssignment]().map { assignment =>
      val referencingLocalVarRefs = getLocalVariableReferencesPointingToAssignment(assignment)
      if (referencingLocalVarRefs.isEmpty) ValidationMessages(s"assignment ${PrettyPrinter.format(assignment).layout} is never used.")
      else ValidationMessages.empty
    })

  def findLocalVarRefsWithoutOrigin: ValidationMessages =
    ValidationMessages.concatenate(getNodes[LocalVariableReferenceExpression]().map { localVarRef =>
      val referencedExpressions = getReferencedVariableAssignmentExpression(localVarRef)
      if (referencedExpressions.isEmpty) ValidationMessages(s"local variable reference '${localVarRef.referenceName}' does not point to any assignment")
      else ValidationMessages.empty
    })

  def findDuplicateVariableAssignments: ValidationMessages =
    ValidationMessages.concatenate(getNodes[VariableAssignment]().map { assignment =>
      val assignmentsWithSameNameInScope = getRightSiblings(assignment).collectFirst {
        case siblingAssignment: VariableAssignment if siblingAssignment.variableName == assignment.variableName =>
          siblingAssignment
      }
      if (assignmentsWithSameNameInScope.isEmpty) ValidationMessages(s"there are multiple assignments with name '${assignment.variableName}' in scope")
      else ValidationMessages.empty
    })

  //</editor-fold>

}
