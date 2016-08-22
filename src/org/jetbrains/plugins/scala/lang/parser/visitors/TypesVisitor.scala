package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.TypesContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object TypesVisitor extends VisitorHelper[TypesContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: TypesContext): Unit = {
    val builder = visitor.builder
    val marker = builder.mark()

    visitor.visitChildren(context)

    if (context.`type`().size() > 1)
      marker.done(ScalaElementTypes.TYPES)
    else
      marker.drop()
  }
}
