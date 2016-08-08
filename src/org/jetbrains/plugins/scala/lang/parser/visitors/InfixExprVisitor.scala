package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.InfixExprContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ErrMsg, ScalaElementTypes, ScalaLangVisitorImpl}
import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils._

/*
infixExpr         : prefixExpr (id typeArgs? Nl? prefixExpr)* ;
 */

object InfixExprVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val context: InfixExprContext = ctx.asInstanceOf[InfixExprContext]

    type MStack[X] = _root_.scala.collection.mutable.Stack[X]

    val markerStack = new MStack[PsiBuilder.Marker]
    val opStack = new MStack[String]
    val infixMarker = builder.mark
    var backupMarker = builder.mark
    var count = 0

    var prefixExprIdx = 0
    val prefixExprCount = context.prefixExpr().size()

    PrefixExprVisitor.visit(visitor, builder, context.prefixExpr(prefixExprIdx), args)
    prefixExprIdx += 1

    while (prefixExprIdx < prefixExprCount) {
      //need to know associativity
      val s = builder.getTokenText

      var exit = false
      while (!exit) {
        if (opStack.isEmpty) {
          opStack push s
          val newMarker = backupMarker.precede
          markerStack push newMarker
          exit = true
        }
        else if (!compar(s, opStack.top, builder)) {
          opStack.pop()
          backupMarker.drop()
          backupMarker = markerStack.top.precede
          markerStack.pop().done(ScalaElementTypes.INFIX_EXPR)
        }
        else {
          opStack push s
          val newMarker = backupMarker.precede
          markerStack push newMarker
          exit = true
        }
      }

      val opMarker = builder.mark
      builder.advanceLexer() //Ate id
      opMarker.done(ScalaElementTypes.REFERENCE_EXPRESSION)
      // TODO: typeArgs
      backupMarker.drop()
      backupMarker = builder.mark

      PrefixExprVisitor.visit(visitor, builder, context.prefixExpr(prefixExprIdx), args)
      prefixExprIdx += 1

      count = count + 1
    }
    backupMarker.drop()
    if (count > 0) {
      while (count > 0 && markerStack.nonEmpty) {
        markerStack.pop().done(ScalaElementTypes.INFIX_EXPR)
        count -= 1
      }

    }
    infixMarker.drop()
    while (markerStack.nonEmpty) {
      markerStack.pop().drop()
    }
  }

  //compares two operators a id2 b id1 c
  private def compar(id1: String, id2: String, builder: PsiBuilder): Boolean = {
    if (priority(id1, assignments = true) < priority(id2, assignments = true)) true //  a * b + c  =((a * b) + c)
    else if (priority(id1, assignments = true) > priority(id2, assignments = true)) false //  a + b * c = (a + (b * c))
    else if (associate(id1) == associate(id2))
      if (associate(id1) == -1) true
      else false
    else {
      builder error ErrMsg("wrong.type.associativity")
      false
    }
  }

  //Associations of operator
  def associate(id: String): Int = {
    id.charAt(id.length - 1) match {
      case ':' => -1
      // right
      case _ => +1 // left
    }
  }
}
