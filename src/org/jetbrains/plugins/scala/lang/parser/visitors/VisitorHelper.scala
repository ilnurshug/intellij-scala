package org.jetbrains.plugins.scala.lang.parser.visitors

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.TerminalNode
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl

trait VisitorHelper[C <: ParserRuleContext] {
  def visit(visitor: ScalaLangVisitorImpl, context: C): Unit
}

object VisitorHelper {
  def hasTerminalNode(context: ParserRuleContext, text: String): Boolean =
    getTerminalNodeIndexByText(context, text) != -1

  def getTerminalNodeIndexByText(context: ParserRuleContext, text: String): Int = {
    import scala.collection.JavaConversions._
    context.children.indexWhere {
      case node: TerminalNode => node.getText == text
      case _ => false
    }
  }
}