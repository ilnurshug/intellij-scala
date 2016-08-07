package org.jetbrains.plugins.scala.lang.parser.visitors

import com.intellij.lang.PsiBuilder
import org.antlr.v4.runtime.ParserRuleContext
import scala.collection.mutable

trait VisitorHelper {
  def visit(builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Object]):Unit
}
