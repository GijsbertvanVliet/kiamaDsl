package gb.kiama.dsl

object ValidationMessages {
  def apply(message: String): ValidationMessages = ValidationMessages(Seq(message))
  def empty: ValidationMessages                  = ValidationMessages(Seq.empty)
  def concatenate(validations: Seq[ValidationMessages]): ValidationMessages =
    validations.foldLeft(ValidationMessages.empty)((prevValidationMessages, nextValidationMessages) => prevValidationMessages ++ nextValidationMessages)
}
case class ValidationMessages(messages: Seq[String]) {
  def ++(next: ValidationMessages): ValidationMessages = ValidationMessages(messages ++ next.messages)
}
