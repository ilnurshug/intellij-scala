package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.CompoundTypeContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object CompoundTypeVisitor extends VisitorHelper[CompoundTypeContext] {

  override def visit(visitor: ScalaLangVisitorImpl, context: CompoundTypeContext): Unit = {
    val marker = visitor.builder.mark()

    visitor.visitChildren(context)

    if (context.refinement() != null || context.annotType().size() > 1)
      marker.done(ScalaElementTypes.COMPOUND_TYPE)
    else
      marker.drop()
  }
}
