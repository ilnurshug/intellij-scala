package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object TypeArgsVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val builder = visitor.getBuilder
    val marker = builder.mark()
    visitor.visitChildren(ctx)
    marker.done(ScalaElementTypes.TYPE_ARGS)
  }
}
