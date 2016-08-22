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
                  | '_'
                  | (thisReference | pathRefExpr)
                  | '(' {disableNewlines();} (exprs ','?)? ')' {restoreNewlinesState();}
                  | (newTemplate | blockExpr) ('.' id)?
                  | simpleExpr1 '_'? '.' id
                  | (newTemplate | blockExpr) (typeArgs)?
                  | simpleExpr1 '_'? typeArgs
                  | simpleExpr1 {!equalTo("(") || !isNl()}? argumentExprs
                  | xmlExpr;

 */

object SimpleExpr1Visitor extends VisitorHelper[SimpleExpr1Context] {
  override def visit(visitor: ScalaLangVisitorImpl, context: SimpleExpr1Context): Unit = {

    if (context.pathRefExpr() != null) {
      //PathVisitor.visit(visitor, builder, context.path(), args)
      visitor.visitPathRefExpr(context.pathRefExpr())
      return
    }

    val marker = visitor.builder.mark()

    val idx: Int = VisitorHelper.getTerminalNodeIndexByText(context, "_")
    idx match {
      case -1 =>    // not found
        visitor.visitChildren(context)
      case _ =>     // found
        val phmarker = visitor.builder.mark()
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

    val childCount:Int = context.getChildCount
    if (childCount > 1) {
      context.getChild(childCount - 1) match {
        case lastChild: RuleNode =>
          val r: Int = lastChild.getRuleContext.getRuleIndex

          r match {
            case  ScalaLangParser.`RULE_argumentExprs` =>
              marker.done (ScalaElementTypes.METHOD_CALL)
            case ScalaLangParser.`RULE_typeArgs` =>
              marker.done (ScalaElementTypes.GENERIC_CALL)
            case ScalaLangParser.`RULE_id` =>
              if (VisitorHelper.hasTerminalNode (context, ".")) marker.done (ScalaElementTypes.REFERENCE_EXPRESSION)
              else marker.drop()
            case _ =>
              marker.drop ()
          }
        case lastChild: TerminalNode =>
          val t: Int = lastChild.getSymbol.getType

          if (t == ScalaLangParser.`RPARENTHESIS`) {
            if (context.exprs() != null) {
              if (VisitorHelper.hasTerminalNode(context, ",") || context.exprs().getChildCount > 1) {
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
