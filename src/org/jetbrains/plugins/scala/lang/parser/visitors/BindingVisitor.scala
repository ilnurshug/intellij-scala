package org.jetbrains.plugins.scala.lang.parser.visitors
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

object BindingVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val builder = visitor.getBuilder
    val paramMarker = builder.mark
    builder.mark.done(ScalaElementTypes.ANNOTATIONS)

    visitor.visitChildren(ctx)

    paramMarker.done(ScalaElementTypes.PARAM)
  }
}
