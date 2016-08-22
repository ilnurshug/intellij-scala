package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.{SimpleTypeContext, SimpleTypeNoMultipleSQBracketsContext, SimpleTypeSubContext}
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
simpleType        : simpleType  typeArgs
                  | simpleType '#' id
                  | simpleTypeSub ;

simpleTypeSub     : stableIdRef
                  | pathRef '.' 'type'
                  | '(' ')'
                  | '('  types ','? ')';
 */

object SimpleTypeVisitor extends VisitorHelper[SimpleTypeContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: SimpleTypeContext): Unit = {
    val builder = visitor.builder

    val marker = builder.mark()

    visitor.visitChildren(context)

    if (context.typeArgs() != null) {
      marker.done(ScalaElementTypes.TYPE_GENERIC_CALL)
    }
    else if (context.id() != null) {
      marker.done(ScalaElementTypes.TYPE_PROJECTION)
    }
    else {
      marker.drop()
    }
  }
}

object SimpleTypeSubVisitor extends VisitorHelper[SimpleTypeSubContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: SimpleTypeSubContext): Unit = {
    val builder = visitor.builder

    val marker = builder.mark()

    visitor.visitChildren(context)

    if (context.getChild(0).getText.compareTo("(") == 0 && context.types() == null) {
      marker.done(ScalaElementTypes.TYPE_IN_PARENTHESIS)
    }
    else if (context.types() != null) {
      val isTuple:Boolean = context.types().`type`().size() > 1
      if (isTuple) marker.done(ScalaElementTypes.TUPLE_TYPE)
      else marker.done(ScalaElementTypes.TYPE_IN_PARENTHESIS)
    }
    else {
      marker.done(ScalaElementTypes.SIMPLE_TYPE)
    }
  }
}

object SimpleTypeNoMultipleSQBracketsVisitor extends VisitorHelper[SimpleTypeNoMultipleSQBracketsContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: SimpleTypeNoMultipleSQBracketsContext): Unit = {
    val builder = visitor.builder

    // simpleTypeNoMultipleSQBrackets '#' id typeArgs
    if (context.id() != null && context.typeArgs() != null) {
      val genericCall = builder.mark()
      val typeProjection = builder.mark()

      visitor.visitSimpleTypeNoMultipleSQBrackets(context.simpleTypeNoMultipleSQBrackets())

      builder.advanceLexer() // ate #
      builder.advanceLexer() // ate id

      typeProjection.done(ScalaElementTypes.TYPE_PROJECTION)

      visitor.visitTypeArgs(context.typeArgs())

      genericCall.done(ScalaElementTypes.TYPE_GENERIC_CALL)
    }
    else {
      val marker = builder.mark()

      visitor.visitChildren(context)

      if (context.typeArgs() != null) {
        marker.done(ScalaElementTypes.TYPE_GENERIC_CALL)
      }
      else if (context.id() != null) {
        marker.done(ScalaElementTypes.TYPE_PROJECTION)
      }
      else {
        marker.drop()
      }
    }

  }
}
