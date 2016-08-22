package org.jetbrains.plugins.scala.lang.parser.visitors
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.BindingContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

object BindingVisitor extends VisitorHelper[BindingContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: BindingContext): Unit = {
    val builder = visitor.builder
    val paramMarker = builder.mark
    builder.mark.done(ScalaElementTypes.ANNOTATIONS)

    visitor.visitChildren(context)

    paramMarker.done(ScalaElementTypes.PARAM)
  }
}
