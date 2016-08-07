package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.tree.ErrorNode
import org.jetbrains.plugins.scala.lang.ScalaLangParser.BlockExprContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object BlockExprVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    /*if (ctx.isInstanceOf[ErrorNode]) {
      ScalaLangVisitorImpl.visitErrorNode(builder, ctx.asInstanceOf[ErrorNode], "blockExprError")
      return
    }*/

    val context: BlockExprContext = ctx.asInstanceOf[BlockExprContext]

    val blockExprMarker = builder.mark
    builder.advanceLexer() // ate '{'

    if (context.block() != null)
      BlockVisitor.visit(visitor, builder, context.block(), args)
    else
      CaseClausesVisitor.visit(visitor, builder, context.caseClauses(), args)

    builder.advanceLexer() // ate '}'

    blockExprMarker.done(ScalaElementTypes.BLOCK_EXPR)
  }
}
