package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object PatternVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val builder = visitor.getBuilder
    val patternMarker = builder.mark

    visitor.visitChildren(ctx)

    if (ctx.getChildCount > 1) patternMarker.done(ScalaElementTypes.PATTERN)
    else patternMarker.drop()
  }
}
