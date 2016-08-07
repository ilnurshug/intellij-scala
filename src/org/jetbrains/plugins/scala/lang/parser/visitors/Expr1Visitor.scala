package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.Expr1Context
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ErrMsg, ScalaElementTypes, ScalaLangVisitorImpl}

/*
expr1             : 'if'  '('  expr  ')'  Nl*  expr ( semi?  'else'  expr)?
                  | 'while'  '('  expr  ')'  Nl*  expr
                  | 'try' ( '{'  block  '}'  |  expr) ( 'catch'  '{'  caseClauses  '}')? ( 'finally'  expr)?
                  | 'do'  expr  semi?  'while'  '('  expr  ')'
                  | 'for'  ('('  enumerators  ')' | '{'  enumerators  '}')  Nl*  'yield'?  expr
                  | 'throw'  expr
                  | 'return'  expr?
                  | postfixExpr '=' expr
                  | postfixExpr
                  | postfixExpr  ascription
                  | postfixExpr  'match'  '{'  caseClauses  '}' ;
 */

object Expr1Visitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val context: Expr1Context = ctx.asInstanceOf[Expr1Context]

    var exprIndex = 0

    val exprMarker = builder.mark
    builder.getTokenType match {
      case ScalaTokenTypes.kIF =>
        visitor.visitChildren(context)
        exprMarker.done(ScalaElementTypes.IF_STMT)

      case ScalaTokenTypes.kWHILE =>
        visitor.visitChildren(context)
        exprMarker.done(ScalaElementTypes.WHILE_STMT)

      case ScalaTokenTypes.kTRY =>

        val tryMarker = builder.mark
        builder.advanceLexer() //Ate try
        builder.getTokenType match {
          case ScalaTokenTypes.tLBRACE =>
            builder.advanceLexer() //Ate {

            args.push(false);
            args.push(false)
            BlockVisitor.visit(visitor, builder, context.block(), args)

            builder.advanceLexer() //Ate }
          case _ =>
            ExprVisitor.visit(visitor, builder, context.expr(exprIndex), args)
            exprIndex = exprIndex + 1
        }

        tryMarker.done(ScalaElementTypes.TRY_BLOCK)

        val catchMarker = builder.mark
        builder.getTokenType match {
          case ScalaTokenTypes.kCATCH =>
            builder.advanceLexer() //Ate catch
            CaseClausesVisitor.visit(visitor, builder, context.caseClauses(), args)
            catchMarker.done(ScalaElementTypes.CATCH_BLOCK)
          case _ =>
            catchMarker.drop()
        }

        val finallyMarker = builder.mark
        builder.getTokenType match {
          case ScalaTokenTypes.kFINALLY =>
            builder.advanceLexer() //Ate finally

            ExprVisitor.visit(visitor, builder, context.expr(exprIndex), args)
            exprIndex = exprIndex + 1

            finallyMarker.done(ScalaElementTypes.FINALLY_BLOCK)
          case _ =>
            finallyMarker.drop()
        }

        exprMarker.done(ScalaElementTypes.TRY_STMT)

      case ScalaTokenTypes.kDO =>
        visitor.visitChildren(context)
        exprMarker.done(ScalaElementTypes.DO_STMT)

      case ScalaTokenTypes.kFOR =>
        visitor.visitChildren(context)
        exprMarker.done(ScalaElementTypes.FOR_STMT)

      case ScalaTokenTypes.kTHROW =>
        visitor.visitChildren(context)
        exprMarker.done(ScalaElementTypes.THROW_STMT)

      case ScalaTokenTypes.kRETURN =>
        visitor.visitChildren(context)
        exprMarker.done(ScalaElementTypes.RETURN_STMT)

      case _ =>
        PostfixExprVisitor.visit(visitor, builder, context.postfixExpr(), args)

        builder.getTokenType match {
          case ScalaTokenTypes.tASSIGN =>
            builder.advanceLexer() //Ate =

            ExprVisitor.visit(visitor, builder, context.expr(exprIndex), args)
            exprIndex = exprIndex + 1

            exprMarker.done(ScalaElementTypes.ASSIGN_STMT)

          case ScalaTokenTypes.tCOLON =>
            AscriptionVisitor.visit(visitor, builder, context.ascription(), args)
            exprMarker.done(ScalaElementTypes.TYPED_EXPR_STMT)

          case ScalaTokenTypes.kMATCH =>
            builder.advanceLexer() //Ate match
            builder.advanceLexer() //Ate {

            CaseClausesVisitor.visit(visitor, builder, context.caseClauses(), args)

            builder.advanceLexer() //Ate }
            exprMarker.done(ScalaElementTypes.MATCH_STMT)
          case _ =>
            exprMarker.drop()
        }
    }
  }
}
