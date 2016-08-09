package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.SimpleTypeContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
simpleType        : simpleType  typeArgs
                  | simpleType '#' id
                  | stableId
                  | path '.' 'type'
                  | '('  types ','? ')';
 */

object SimpleTypeVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val context = ctx.asInstanceOf[SimpleTypeContext]
    val builder = visitor.getBuilder

    val marker = builder.mark()

    visitor.visitChildren(context)

    if (context.typeArgs() != null) {
      marker.done(ScalaElementTypes.TYPE_GENERIC_CALL)
    }
    else if (context.id() != null) {
      marker.done(ScalaElementTypes.TYPE_PROJECTION)
    }
    else if (context.types() != null) {
      val isTuple:Boolean = context.types().getChildCount > 1
      if (isTuple) marker.done(ScalaElementTypes.TUPLE_TYPE)
      else marker.done(ScalaElementTypes.TYPE_IN_PARENTHESIS)
    }
    else {
      marker.done(ScalaElementTypes.SIMPLE_TYPE)
    }
  }
}
