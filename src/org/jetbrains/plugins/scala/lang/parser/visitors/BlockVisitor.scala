package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.BlockContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

object BlockVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val builder = visitor.getBuilder
    val args = visitor.args

    val context:BlockContext = ctx.asInstanceOf[BlockContext]
    if (args.isEmpty) {
      visitor.visitChildren(context)
    }
    else {
      val needNode = args.pop()
      val hasBrace = args.pop()

      if (hasBrace) {
        val blockMarker = builder.mark
        builder.advanceLexer() // ate '{'

        visit(visitor, context)

        builder.advanceLexer() // ate '}'

        blockMarker.done(ScalaElementTypes.BLOCK_EXPR)
      }
      else {
        val bm = builder.mark()
        var count = context.blockStat().size()
        if (context.resultExpr() != null) count = count + 1

        visit(visitor, context)

        if (count > 1) {
          bm.done(ScalaElementTypes.BLOCK)
        } else {
          if (!needNode) bm.drop() else bm.done(ScalaElementTypes.BLOCK)
        }
      }
    }
  }
}
