package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.{AnnotTypeContext, AnnotTypeNoMultipleSQBracketsContext}
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object AnnotTypeVisitor extends VisitorHelper[AnnotTypeContext] {

  override def visit(visitor: ScalaLangVisitorImpl, context: AnnotTypeContext): Unit = {
    val marker = visitor.builder.mark()

    visitor.visitChildren(context)

    if (context.annotationsNonEmpty() != null)
      marker.done(ScalaElementTypes.ANNOT_TYPE)
    else
      marker.drop()
  }
}


object AnnotTypeNoMultipleSQBracketsVisitor extends VisitorHelper[AnnotTypeNoMultipleSQBracketsContext] {

  override def visit(visitor: ScalaLangVisitorImpl, context: AnnotTypeNoMultipleSQBracketsContext): Unit = {
    val marker = visitor.builder.mark()

    visitor.visitChildren(context)

    if (context.annotationsNonEmpty() != null)
      marker.done(ScalaElementTypes.ANNOT_TYPE)
    else
      marker.drop()
  }
}