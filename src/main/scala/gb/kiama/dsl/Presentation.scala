package gb.kiama.dsl

import org.bitbucket.inkytonik.kiama.attribution.UncachedAttribution.attr
import org.bitbucket.inkytonik.kiama.relation.Tree

object Presentation {
  import MyTree._

  val threePlusTwo         = AddExpression(IntegerLiteralExpression(3), IntegerLiteralExpression(2))
  val fivePlusThreePlusTwo = AddExpression(IntegerLiteralExpression(5), threePlusTwo)

  type MyTree = Tree[TreeNode, TreeNode]
//  val tree = new MyTree(fivePlusThreePlusTwo)
//
//  val parent        = tree.parent(threePlusTwo)
//  val children      = tree.child(threePlusTwo)
//  val leftSiblings  = tree.prev(threePlusTwo)
//  val rightSiblings = tree.next(threePlusTwo)

  val tree = new MyTree(fivePlusThreePlusTwo)

  val getResult: TreeNode => Int =
    attr {
      case IntegerLiteralExpression(int) => int
      case AddExpression(left, right) =>
        getResult(left) + getResult(right)
    }

  val getParents: TreeNode => Seq[TreeNode] =
    attr {
      case tree.parent(p) => p +: getParents(p)
      case _              => Seq.empty
    }

  """
    | root(input: String, secondInput: Int) {
    |   val a: String = "something";
    |   val b: Int = 1 + -2;
    |   val c: Int = secondInput;
    |   return a + " hello" + input;
    | }
    |""".stripMargin

  """
    | root(input: String, secondInput: Int) {
    |   val a: String = "something";
    |   return a + " hello" + input;
    | }
    |""".stripMargin

  """
    | root(input: String, secondInput: Int) {
    |   val a: String = "something";
    |   return "something" + " hello" + input;
    | }
    |""".stripMargin

  """
    | root(input: String, secondInput: Int) {
    |   val a: String = "something";
    |   return "something hello" + input;
    | }
    |""".stripMargin
}
