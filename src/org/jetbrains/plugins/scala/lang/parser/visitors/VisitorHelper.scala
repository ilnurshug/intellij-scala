package org.jetbrains.plugins.scala.lang.parser.visitors

import com.intellij.lang.PsiBuilder
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ParseTreeVisitor
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl

import scala.collection.mutable

trait VisitorHelper {
  def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]):Unit
}
