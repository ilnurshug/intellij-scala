package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.PatternContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object PatternVisitor extends VisitorHelper[PatternContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: PatternContext): Unit = {
    val builder = visitor.builder
    val patternMarker = builder.mark

    visitor.visitChildren(context)

    if (context.getChildCount > 1) patternMarker.done(ScalaElementTypes.PATTERN)
    else patternMarker.drop()
  }
}
