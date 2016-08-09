package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.ExprContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ErrMsg, ScalaElementTypes, ScalaLangVisitorImpl}

/*
expr              : (bindings | id | '_')  '=>'  expr
                  | expr1 ;
 */

object ExprVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val context: ExprContext = ctx.asInstanceOf[ExprContext]
    val builder = visitor.getBuilder

    if (context.expr1() != null) {
      visitor.visitChildren(context.expr1())
      return
    }

    val exprMarker = builder.mark

    builder.getTokenType match {
      case ScalaTokenTypes.tIDENTIFIER | ScalaTokenTypes.tUNDER =>
        val pmarker = builder.mark

        builder.advanceLexer() //Ate id

        builder.getTokenType match {
          case ScalaTokenTypes.tFUNTYPE =>
            val psm = pmarker.precede // 'parameter clause'
            val pssm = psm.precede // 'parameter list'
            pmarker.done(ScalaElementTypes.PARAM)
            psm.done(ScalaElementTypes.PARAM_CLAUSE)
            pssm.done(ScalaElementTypes.PARAM_CLAUSES)

            builder.advanceLexer() //Ate =>
            //if (!parse(builder)) builder error ErrMsg("wrong.expression")
            visit(visitor, context.expr())

            exprMarker.done(ScalaElementTypes.FUNCTION_EXPR)
            return
          case _ =>
            pmarker.drop()
            exprMarker.rollbackTo()
        }

      case ScalaTokenTypes.tLPARENTHESIS =>

        //BindingsVisitor.visit(visitor, builder, context.bindings(), args)
        visitor.visitBindings(context.bindings())

        builder.advanceLexer() //Ate =>
        visit(visitor, context.expr())
        exprMarker.done(ScalaElementTypes.FUNCTION_EXPR)
        return

      case _ => exprMarker.drop()
    }

  }
}