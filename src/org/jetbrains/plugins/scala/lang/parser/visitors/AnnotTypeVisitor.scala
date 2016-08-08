package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.AnnotTypeContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object AnnotTypeVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val context = ctx.asInstanceOf[AnnotTypeContext]
    val marker = visitor.getBuilder.mark()

    visitor.visitChildren(ctx)

    if (context.annotationsNonEmpty() != null)
      marker.done(ScalaElementTypes.ANNOT_TYPE)
    else
      marker.drop()
  }
}
