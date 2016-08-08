package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.PrefixExprContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
prefixExpr        : ('-' | '+' | '~' | '!')? simpleExpr ;
 */

object PrefixExprVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val builder = visitor.getBuilder
    val context: PrefixExprContext = ctx.asInstanceOf[PrefixExprContext]

    builder.getTokenText match {
      case "-" | "+" | "~" | "!" =>
        val prefixMarker = builder.mark
        val refExpr = builder.mark
        builder.advanceLexer()
        refExpr.done(ScalaElementTypes.REFERENCE_EXPRESSION)

        //SimpleExprVisitor.visit(visitor, builder, context.simpleExpr(), args)
        visitor.visitSimpleExpr(context.simpleExpr())

        prefixMarker.done(ScalaElementTypes.PREFIX_EXPR);

      case _ => visitor.visitChildren(context)
    }
  }
}
