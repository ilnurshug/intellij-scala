package org.jetbrains.plugins.scala.lang.parser.visitors

import com.intellij.lang.PsiBuilder
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{ParseTreeVisitor, TerminalNode}
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl

import scala.collection.mutable

trait VisitorHelper {
  def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]):Unit

  def hasTerminalNode(ctx: ParserRuleContext, s: String): Boolean = {
    for (i <- 0 until ctx.getChildCount)
      if (ctx.getChild(i).isInstanceOf[TerminalNode]) {
        if (ctx.getChild(i).getText.compareTo(s) == 0) return true
      }

    false
  }
}
