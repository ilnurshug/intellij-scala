package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object StableIdVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val builder = visitor.getBuilder
    // TODO: rewrite this (see details in stableId.scala)
    val marker = builder.mark()
    visitor.visitChildren(ctx)
    marker.done(ScalaElementTypes.REFERENCE)
  }
}
