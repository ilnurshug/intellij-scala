package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.{InfixExprContext, PostfixExprContext}
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
postfixExpr       : infixExpr ( id  Nl?)? ;
 */

object PostfixExprVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val context: PostfixExprContext = ctx.asInstanceOf[PostfixExprContext]
    if (context.id() == null) {
      visitor.visitChildren(context)
      return
    }

    val postfixMarker = builder.mark

    InfixExprVisitor.visit(visitor, builder, context.infixExpr(), args)

    val refMarker = builder.mark
    builder.advanceLexer() //Ate id
    refMarker.done(ScalaElementTypes.REFERENCE_EXPRESSION)

    postfixMarker.done(ScalaElementTypes.POSTFIX_EXPR)
  }
}
