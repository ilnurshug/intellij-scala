package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.TypesContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object TypesVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val context = ctx.asInstanceOf[TypesContext]
    val builder = visitor.getBuilder
    val marker = builder.mark()

    visitor.visitChildren(ctx)

    if (context.`type`().size() > 1)
      marker.done(ScalaElementTypes.TYPES)
    else
      marker.drop()
  }
}
