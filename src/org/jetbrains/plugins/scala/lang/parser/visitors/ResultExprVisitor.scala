package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.ResultExprContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ErrMsg, ScalaElementTypes, ScalaLangVisitorImpl}

/*
resultExpr        : bindings  '=>'  blockNode
                  | ('implicit'?  id | '_')  ':'  paramType '=>'  blockNode ;
 */

object ResultExprVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val context = ctx.asInstanceOf[ResultExprContext]
    val builder = visitor.getBuilder

    val resultMarker = builder.mark
    val backupMarker = builder.mark

    def parseFunctionEnd() = builder.getTokenType match {
      case ScalaTokenTypes.tFUNTYPE =>
        builder.advanceLexer() //Ate =>
        //block parse(builder, hasBrace = false, needNode = true)
        visitor.visitBlockNode(context.blockNode())
        backupMarker.drop()
        resultMarker.done(ScalaElementTypes.FUNCTION_EXPR)
        true
      case _ =>
        resultMarker.drop()
        backupMarker.rollbackTo()
        false
    }

    def parseFunction(paramsMarker: PsiBuilder.Marker): Boolean = {
      val paramMarker = builder.mark()
      builder.advanceLexer() //Ate id
      if (ScalaTokenTypes.tCOLON == builder.getTokenType) {
        builder.advanceLexer() // ate ':'
        val pt = builder.mark
        //`type`.parse(builder, isPattern = false)
        visitor.visitCompoundType(context.compoundType())
        pt.done(ScalaElementTypes.PARAM_TYPE)
      }
      builder.getTokenType match {
        case ScalaTokenTypes.tFUNTYPE =>
          val psm = paramsMarker.precede // 'parameter list'
          paramMarker.done(ScalaElementTypes.PARAM)
          paramsMarker.done(ScalaElementTypes.PARAM_CLAUSE)
          psm.done(ScalaElementTypes.PARAM_CLAUSES)

          return parseFunctionEnd()
        case _ =>
          builder error ErrMsg("fun.sign.expected")
      }
      parseFunctionEnd()
    }


    builder.getTokenType match {
      case ScalaTokenTypes.tLPARENTHESIS =>
        //bindings parse builder
        visitor.visitBindings(context.bindings())
        parseFunctionEnd()
        return
      case ScalaTokenTypes.kIMPLICIT =>
        val pmarker = builder.mark()
        builder.advanceLexer() //ate implicit
        builder.getTokenType match {
          case ScalaTokenTypes.tIDENTIFIER =>
            parseFunction(pmarker)
            return
          case _ =>
            resultMarker.drop()
            backupMarker.rollbackTo()
            return
        }
      case ScalaTokenTypes.tIDENTIFIER | ScalaTokenTypes.tUNDER =>
        val pmarker = builder.mark
        parseFunction(pmarker)
        return
      case _ =>
        backupMarker.drop()
    }
    resultMarker.drop()
  }
}
