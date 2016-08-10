package org.jetbrains.plugins.scala.lang.parser.visitors

import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTreeVisitor, TerminalNode}
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl

trait VisitorHelper {
  def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext):Unit

  def hasTerminalNode(ctx: ParserRuleContext, s: String): Boolean = {
    for (i <- 0 until ctx.getChildCount)
      if (ctx.getChild(i).isInstanceOf[TerminalNode]) {
        if (ctx.getChild(i).getText.compareTo(s) == 0) return true
      }

    false
  }

  def getTerminalNodeIndexByText(ctx: ParserRuleContext, s: String): Int = {
    for (i <- 0 until ctx.getChildCount)
      if (ctx.getChild(i).isInstanceOf[TerminalNode]) {
        if (ctx.getChild(i).getText.compareTo(s) == 0) return i
      }

    -1
  }
}
