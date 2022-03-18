package gb.kiama.dsl

import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.util.Positions

object MyTree {

  type MyTree = Tree[TreeNode, Root]

  sealed trait TreeNode extends Product {
    protected def returnTypeValidations(results: Either[ValidationMessages, DataType]*): ValidationMessages =
      ValidationMessages.concatenate(
        results
          .collect { case Left(validationMessages) =>
            validationMessages
          }
      )
  }

  sealed abstract class Expression extends TreeNode {
    def reduce: Expression = this
    def getReturnType(localScope: Seq[VariableAssignment])(implicit position: Positions): Either[ValidationMessages, DataType]

  }

  sealed abstract class LiteralExpression[T](value: T) extends Expression
  case class IntegerLiteralExpression(value: Integer) extends LiteralExpression[Integer](value) {
    override def getReturnType(localScope: Seq[VariableAssignment])(implicit position: Positions): Either[ValidationMessages, DataType] = Right(IntegerType)
  }
  case class StringLiteralExpression(value: String) extends LiteralExpression[String](value) {
    override def getReturnType(localScope: Seq[VariableAssignment])(implicit position: Positions): Either[ValidationMessages, DataType] = Right(StringType)
  }

  sealed abstract class BinaryExpression(left: Expression, right: Expression) extends Expression

  case class AddExpression(left: Expression, right: Expression) extends BinaryExpression(left, right) {
    override def reduce: Expression = (left, right) match {
      case (leftString: StringLiteralExpression, rightString: StringLiteralExpression) =>
        StringLiteralExpression(leftString.value + rightString.value)
      case (leftInt: IntegerLiteralExpression, rightInt: IntegerLiteralExpression) =>
        IntegerLiteralExpression(leftInt.value + rightInt.value)
      case _ => super.reduce
    }

    override def getReturnType(localScope: Seq[VariableAssignment])(implicit position: Positions): Either[ValidationMessages, DataType] =
      (left.getReturnType(localScope), right.getReturnType(localScope)) match {
        case (Right(StringType), Right(StringType))    => Right(StringType)
        case (Right(IntegerType), Right(IntegerType))  => Right(IntegerType)
        case (_ @Right(leftType), _ @Right(rightType)) => Left(ValidationMessages(s"Cannot add a $leftType to a $rightType", Error, this))
        case (leftResult, rightResult)                 => Left(returnTypeValidations(leftResult, rightResult))

      }
  }

  case class SubExpression(left: Expression, right: Expression) extends BinaryExpression(left, right) {
    override def reduce: Expression = (left, right) match {
      case (leftInt: IntegerLiteralExpression, rightInt: IntegerLiteralExpression) =>
        IntegerLiteralExpression(leftInt.value + rightInt.value)
      case _ => super.reduce
    }

    override def getReturnType(localScope: Seq[VariableAssignment])(implicit position: Positions): Either[ValidationMessages, DataType] =
      (left.getReturnType(localScope), right.getReturnType(localScope)) match {
        case (Right(IntegerType), Right(IntegerType))  => Right(IntegerType)
        case (_ @Right(leftType), _ @Right(rightType)) => Left(ValidationMessages(s"Cannot substract a $leftType to a $rightType.", Error, this))
        case (leftResult, rightResult)                 => Left(returnTypeValidations(leftResult, rightResult))
      }
  }

  sealed abstract class UnaryExpression(exp: Expression) extends Expression
  case class NegateExpression(exp: Expression) extends UnaryExpression(exp) {
    override def reduce: Expression = exp match {
      case intLiteral: IntegerLiteralExpression => IntegerLiteralExpression(-(intLiteral.value))
      case _                                    => super.reduce
    }

    override def getReturnType(localScope: Seq[VariableAssignment])(implicit position: Positions): Either[ValidationMessages, DataType] =
      exp.getReturnType(localScope) match {
        case Right(IntegerType)                           => Right(IntegerType)
        case Right(StringType)                            => Left(ValidationMessages("Not able to compute the negation of a string", Error, this))
        case Left(validationMessages: ValidationMessages) => Left(validationMessages)
      }
  }

  case class LocalVariableReferenceExpression(referenceName: String) extends Expression {
    override def getReturnType(localScope: Seq[VariableAssignment])(implicit position: Positions): Either[ValidationMessages, DataType] =
      localScope.find(_.variableName == referenceName) match {
        case None                     => Left(ValidationMessages(s"There is no local variable with name $referenceName in scope", Error, this))
        case Some(variableAssignment) => Right(variableAssignment.dataType)
      }
  }

  sealed trait DataType   extends TreeNode
  case object StringType  extends DataType
  case object IntegerType extends DataType

  sealed abstract class VariableAssignment(val dataType: DataType, val variableName: String) extends TreeNode

  sealed trait Statement extends TreeNode
  case class VariableAssignmentStatement(override val dataType: DataType, override val variableName: String, exp: Expression)
      extends VariableAssignment(dataType, variableName)
      with Statement

  case class Return(expression: Expression) extends Statement

  case class InputArgument(override val dataType: DataType, override val variableName: String) extends VariableAssignment(dataType, variableName)

  case class Root(inputArguments: Seq[InputArgument], statements: Seq[Statement], returnStatement: Return) extends TreeNode
}
