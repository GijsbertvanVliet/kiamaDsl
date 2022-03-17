package gb.kiama.dsl

import gb.kiama.dsl.MyTree._
import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document

class PrettyPrinter extends org.bitbucket.inkytonik.kiama.output.PrettyPrinter {

  /**
   * Format an imperative node.
   */
  def format(t: TreeNode): Document =
    pretty(toDoc(t))

  def toDoc(t: TreeNode): Doc = t match {
    case IntegerLiteralExpression(int)                       => value(int)
    case StringLiteralExpression(string)                     => "\"" <> value(string) <> "\""
    case AddExpression(left, right)                          => binToDoc(left, "+", right)
    case SubExpression(left, right)                          => binToDoc(left, "-", right)
    case NegateExpression(exp)                               => "-" <> toDoc(exp)
    case LocalVariableReferenceExpression(refName)           => value(refName)
    case StringType                                          => "String"
    case IntegerType                                         => "Int"
    case VariableAssignmentStatement(dataType, varName, exp) => "val" <+> varName <> ":" <+> toDoc(dataType) <+> "=" <+> toDoc(exp) <> ";"
    case Return(exp)                                         => "return" <+> toDoc(exp) <> ";"
    case InputArgument(dataType, varName)                    => value(varName) <> ":" <+> toDoc(dataType)
    case Root(inputArguments, statements, returnStatement) =>
      "root" <+> group(parens(nest(ssep(inputArguments.map(toDoc), "," <> line)))) <+>
        group(braces(nest(line <> ssep((statements :+ returnStatement).map(toDoc), line)) <> linebreak))
  }

  def binToDoc(left: TreeNode, separator: String, right: TreeNode): Doc =
    toDoc(left) <+> separator <+> toDoc(right)
}

object PrettyPrinter extends PrettyPrinter
