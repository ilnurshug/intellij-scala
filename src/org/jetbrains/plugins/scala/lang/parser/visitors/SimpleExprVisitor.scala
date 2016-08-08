package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
simpleExpr        : 'new' (classTemplate | templateBody) | blockExpr | simpleExpr1 '_'?;
 */

object SimpleExprVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val simpleMarker = builder.mark

    builder.getTokenType match {
      case ScalaTokenTypes.kNEW =>
        visitor.visitChildren(ctx)
        simpleMarker.done(ScalaElementTypes.NEW_TEMPLATE)
      case _ =>
        visitor.visitChildren(ctx)
        simpleMarker.drop()
    }
  }
}
