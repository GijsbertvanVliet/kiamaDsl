package gb.kiama.dsl

import org.bitbucket.inkytonik.kiama.parsing.Parsers
import org.bitbucket.inkytonik.kiama.util.Positions

class SyntaxAnalyser(positions: Positions) extends Parsers(positions) {
  import MyTree._

  lazy val rootParser: Parser[Root] =
    "root" ~> ("(" ~> repsep(inputArgument, ",") <~ ")") ~ ("{" ~> rep(statement) ~ returnStatement <~ "}") ^^ {
      case capturedInputArguments ~ (capturedStatements ~ capturedReturn) =>
        Root.apply(capturedInputArguments, capturedStatements, capturedReturn)
    }

  lazy val inputArgument: Parser[InputArgument] =
    (id <~ ":") ~ dataType ^^ { case capturedId ~ capturedDataType =>
      InputArgument.apply(capturedDataType, capturedId)
    }

  lazy val statement: Parser[Statement] =
    compoundStatement | varAssignmentStatement

  lazy val returnStatement: Parser[Return] =
    "return" ~> exp <~ ";" ^^ Return.apply

  lazy val compoundStatement: Parser[CompoundStatement] =
    "{" ~> rep(varAssignmentStatement) <~ "}" ^^ CompoundStatement.apply

  lazy val varAssignmentStatement: Parser[VariableAssignmentStatement] =
    "val" ~> id ~ ((":" ~> dataType <~ "=") ~ exp <~ ";") ^^ { case capturedId ~ (capturedDataType ~ capturedExpression) =>
      VariableAssignmentStatement.apply(capturedDataType, capturedId, capturedExpression)
    }

  lazy val exp: PackratParser[Expression] =
    exp ~ ("+" ~> factor) ^^ AddExpression.apply |
      exp ~ ("-" ~> factor) ^^ SubExpression.apply |
      factor

  lazy val factor: Parser[Expression] =
    localVariable | intLiteral | stringLiteral | "-" ~> exp ^^ (expression => NegateExpression(expression)) | "(" ~> exp <~ ")"

  lazy val localVariable: Parser[LocalVariableReferenceExpression] =
    "[a-zA-Z]+".r ^^ (referenceName => LocalVariableReferenceExpression(referenceName))

  lazy val intLiteral: Parser[IntegerLiteralExpression] =
    "[0-9]+".r ^^ (i => IntegerLiteralExpression(i.toInt))

  lazy val stringLiteral: Parser[StringLiteralExpression] =
    "\"[^\"]*\"".r ^^ (s => StringLiteralExpression(s.substring(1, s.length - 1)))

  lazy val id: Parser[String] =
    "[a-zA-Z]+".r

  lazy val dataType = stringDataType | integerDataType

  lazy val stringDataType: Parser[StringType.type] =
    "String".r ^^ (_ => StringType)

  lazy val integerDataType: Parser[IntegerType.type] =
    "Int".r ^^ (_ => IntegerType)

}

