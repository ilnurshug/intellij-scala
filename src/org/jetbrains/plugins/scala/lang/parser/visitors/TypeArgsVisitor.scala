package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.TypeArgsContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
typeArgs          : '['  type ( ','  type)*  ']';
 */

object TypeArgsVisitor extends VisitorHelper[TypeArgsContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: TypeArgsContext): Unit = {
    val builder = visitor.builder
    val marker = builder.mark()

    if (!visitor.isPattern) {
      visitor.visitChildren(context)
    }
    else {
      builder.advanceLexer() // ate '['

      def checkTypeVariable: Boolean = {
        builder.getTokenType match {
          case ScalaTokenTypes.tIDENTIFIER =>
            val idText = builder.getTokenText
            val firstChar = idText.charAt(0)
            if (firstChar != '`' && firstChar.isLower) {
              val typeParameterMarker = builder.mark()
              val idMarker = builder.mark()
              builder.advanceLexer()
              builder.getTokenType match {
                case ScalaTokenTypes.tCOMMA | ScalaTokenTypes.tRSQBRACKET =>
                  idMarker.drop()
                  typeParameterMarker.done(ScalaElementTypes.TYPE_VARIABLE)
                  true
                case _ =>
                  idMarker.rollbackTo()
                  typeParameterMarker.drop()
                  false
              }
            } else false
          case _ => false
        }
      }

      var typeIdx:Int = 0
      val typeCount:Int = context.`type`().size()

      while (typeIdx < typeCount) {
        if (!checkTypeVariable) {
          visitor.visitType(context.`type`(typeIdx))
        }

        typeIdx += 1

        if (typeIdx < typeCount) builder.advanceLexer() // ate ','
      }

      builder.advanceLexer() // ate ']'
    }

    marker.done(ScalaElementTypes.TYPE_ARGS)
  }
}
