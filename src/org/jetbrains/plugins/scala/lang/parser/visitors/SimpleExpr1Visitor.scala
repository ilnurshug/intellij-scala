package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.{RuleNode, TerminalNode}
import org.jetbrains.plugins.scala.lang.ScalaLangParser
import org.jetbrains.plugins.scala.lang.ScalaLangParser.SimpleExpr1Context
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
simpleExpr1       : literal
                  | path
                  | '_'
                  | '(' (exprs ','?)? ')'
                  | ('new' (classTemplate | templateBody) | blockExpr) '.' id
                  | simpleExpr1 '_'? '.' id
                  | ('new' (classTemplate | templateBody) | blockExpr) typeArgs
                  | simpleExpr1 '_'? typeArgs
                  | simpleExpr1 argumentExprs ;

 */

object SimpleExpr1Visitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val context: SimpleExpr1Context = ctx.asInstanceOf[SimpleExpr1Context]

    if (context.path() != null) {
      visitor.typeArgs.push(ScalaElementTypes.REFERENCE_EXPRESSION)
      PathVisitor.visit(visitor, builder, context.path(), args)
      return
    }

    val marker = builder.mark()

    visitor.visitChildren(ctx)

    val childCount = ctx.getChildCount
    if (childCount > 1) {
      if (ctx.getChild(childCount - 1).isInstanceOf[RuleNode]) {
        val lastChild = ctx.getChild(childCount - 1).asInstanceOf[RuleNode]
        val r = lastChild.getRuleContext.getRuleIndex

        if (r == ScalaLangParser.RULE_argumentExprs) {
          marker.done(ScalaElementTypes.METHOD_CALL)
        } else if (r == ScalaLangParser.RULE_typeArgs) {
          marker.done(ScalaElementTypes.GENERIC_CALL)
        } else if (r == ScalaLangParser.RULE_id && hasTerminalNode(ctx, ".")) {
          marker.done(ScalaElementTypes.REFERENCE_EXPRESSION)
        } else {
          marker.drop()
        }
      }
      else {
        val lastChild = ctx.getChild(childCount - 1).asInstanceOf[TerminalNode]
        val t = lastChild.getSymbol.getType
        if (t == ScalaLangParser.RPARENTHESIS) {
          if (context.exprs() != null) {
            if (hasTerminalNode(ctx, ",")) {
              marker.done(ScalaElementTypes.TUPLE)
            }
            else {
              marker.done(ScalaElementTypes.PARENT_EXPR)
            }
          }
          else {
            marker.done(ScalaElementTypes.UNIT_EXPR)
          }
        }
        else marker.drop()
      }
    }
    else {
      marker.drop()
    }
  }
}
