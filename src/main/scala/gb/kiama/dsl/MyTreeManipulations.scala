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

  def getNodes[T <: TreeNode: ClassTag]: Seq[T] = collectl { case node: T =>
    node
  }(tree.root)

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

  val getReferencedVariableAssignment: LocalVariableReferenceExpression => Option[Expression] = attr { node: LocalVariableReferenceExpression =>
    getAllLocalVariablesInScope(node).find(_.variableName == node.referenceName).collect { case assignmentStatement: VariableAssignmentStatement =>
      assignmentStatement.exp
    }
  }

  val reduceBinaryAndUnaryExpressionStrategy: Strategy = rule[TreeNode] {
    case binaryExpression: BinaryExpression => binaryExpression.reduce
    case unaryExpression: UnaryExpression   => unaryExpression.reduce
  }

  val replaceLocalVariableReferencesStrategy: Strategy = strategy[TreeNode] { case localVarRef: LocalVariableReferenceExpression =>
    getReferencedVariableAssignment(localVarRef)
  }

  val resolveUnariesAndBinaries: Root = rewrite(bottomup(reduceBinaryAndUnaryExpressionStrategy <+ id))(tree.root)

  val resolveLocalVariables: Root = rewrite(reduce(replaceLocalVariableReferencesStrategy))(tree.root)

  //<editor-fold defaultstate="collapsed"  desc="validations">

  private def checkVariableAssignmentsDataType: ValidationMessages = ValidationMessages.concatenate(
    getNodes[VariableAssignmentStatement]
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

  //</editor-fold>

}