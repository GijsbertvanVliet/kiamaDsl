package gb.kiama.dsl

import gb.kiama.dsl.MyTree.TreeNode
import org.bitbucket.inkytonik.kiama.util.Positions

sealed trait ValidationErrorLevel
object Warning extends ValidationErrorLevel {
  override def toString: String = "Warning"
}
object Error extends ValidationErrorLevel {
  override def toString: String = "Error"
}

object ValidationMessages {
  def apply(message: String, errorLevel: ValidationErrorLevel, node: TreeNode)(implicit positions: Positions): ValidationMessages = ValidationMessages(
    Seq(ValidationMessage(message, errorLevel, node))
  )
  def empty: ValidationMessages = ValidationMessages(Seq.empty)
  def concatenate(validations: Seq[ValidationMessages]): ValidationMessages =
    validations.foldLeft(ValidationMessages.empty)((prevValidationMessages, nextValidationMessages) => prevValidationMessages ++ nextValidationMessages)
}
case class ValidationMessages(messages: Seq[ValidationMessage]) {
  def ++(next: ValidationMessages): ValidationMessages = ValidationMessages(messages ++ next.messages)

  override def toString: String = messages.mkString(s";${System.lineSeparator}")
}

object ValidationMessage {
  def apply(message: String, errorLevel: ValidationErrorLevel, treeNode: TreeNode)(implicit positions: Positions): ValidationMessage =
    ValidationMessage(message, errorLevel, Some(treeNode))
}

case class ValidationMessage(message: String, errorLevel: ValidationErrorLevel, maybeNode: Option[TreeNode] = None)(implicit positions: Positions) {
  override def toString: String = maybeNode match {
    case None           => s"$errorLevel => $message"
    case Some(treeNode) => s"$errorLevel => $message; origin=${positions.textOf(treeNode).getOrElse("Could not find origin positions!")}"
  }
}
