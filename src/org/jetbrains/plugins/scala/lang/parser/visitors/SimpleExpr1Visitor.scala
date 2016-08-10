package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder
import com.intellij.psi.tree.IElementType

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
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val context: SimpleExpr1Context = ctx.asInstanceOf[SimpleExpr1Context]

    if (context.pathRefExpr() != null) {
      //PathVisitor.visit(visitor, builder, context.path(), args)
      visitor.visitPathRefExpr(context.pathRefExpr())
      return
    }

    val marker = visitor.getBuilder.mark()

    val idx: Int = getTerminalNodeIndexByText(context, "_")
    idx match {
      case -1 =>    // not found
        visitor.visitChildren(ctx)
      case _ =>     // found
        val phmarker = visitor.getBuilder.mark()
        var i:Int = 0
        while (i <= idx) {
          context.getChild(i).accept(visitor)
          i += 1
        }
        phmarker.done(ScalaElementTypes.PLACEHOLDER_EXPR)
        while (i < context.getChildCount) {
          context.getChild(i).accept(visitor)
          i += 1
        }
    }

    val childCount:Int = ctx.getChildCount
    if (childCount > 1) {
      if (ctx.getChild(childCount - 1).isInstanceOf[RuleNode]) {
        val lastChild = ctx.getChild(childCount - 1).asInstanceOf[RuleNode]
        val r: Int = lastChild.getRuleContext.getRuleIndex

        r match {
          case  ScalaLangParser.RULE_argumentExprs =>
            marker.done (ScalaElementTypes.METHOD_CALL)
          case ScalaLangParser.RULE_typeArgs =>
            marker.done (ScalaElementTypes.GENERIC_CALL)
          case ScalaLangParser.RULE_id  =>
            if (hasTerminalNode (ctx, ".")) marker.done (ScalaElementTypes.REFERENCE_EXPRESSION)
            else marker.drop()
          case _ =>
            marker.drop ()
        }
      }
      else {
        val lastChild = ctx.getChild(childCount - 1).asInstanceOf[TerminalNode]
        val t: Int = lastChild.getSymbol.getType
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
