package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.PathContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object PathVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val element = visitor.typeArgs.pop()
    val context = ctx.asInstanceOf[PathContext]

    if (context.stableId() != null) {
      visitor.typeArgs.push(element)
      StableIdVisitor.visit(visitor, builder, context.stableId(), args)
    }
    else {
      val marker = builder.mark()
      visitor.visitChildren(context)
      marker.done(ScalaElementTypes.THIS_REFERENCE)
    }
  }
}
