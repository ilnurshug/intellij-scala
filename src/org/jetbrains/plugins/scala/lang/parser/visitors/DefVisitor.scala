package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.DefContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
def  : annotations modifiersOrEmpty ('val'  patDef | 'var'  varDef | 'def'  funDef | 'type'  Nl*  typeDef)
     | tmplDef ;

 */

object DefVisitor extends VisitorHelper {

  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val context: DefContext = ctx.asInstanceOf[DefContext]

    val defMarker = builder.mark

    AnnotationsVisitor.visit(visitor, builder, context.annotations(), args)
    ModifiersOrEmptyVisitor.visit(visitor, builder, context.modifiersOrEmpty(), args)

    builder.getTokenType match {
      case ScalaTokenTypes.kVAL =>
        builder.advanceLexer() //Ate val
        PatDefVisitor.visit(visitor, builder, context.patDef(), args)
        defMarker.done(ScalaElementTypes.PATTERN_DEFINITION)

      case ScalaTokenTypes.kVAR =>
        builder.advanceLexer() //Ate var
        VarDefVisitor.visit(visitor, builder, context.varDef(), args)
        defMarker.done(ScalaElementTypes.VARIABLE_DEFINITION)

      case ScalaTokenTypes.kDEF =>
        builder.advanceLexer() //Ate def
        FunDefVisitor.visit(visitor, builder, context.funDef(), args)
        defMarker.done(ScalaElementTypes.FUNCTION_DEFINITION)

      case ScalaTokenTypes.kTYPE =>
        builder.advanceLexer() //Ate type
        TypeDefVisitor.visit(visitor, builder, context.typeDef(), args)
        defMarker.done(ScalaElementTypes.TYPE_DEFINITION)

      case ScalaTokenTypes.kCASE | ScalaTokenTypes.kCLASS
           | ScalaTokenTypes.kOBJECT | ScalaTokenTypes.kTRAIT =>
        defMarker.rollbackTo()
        TmplDefVisitor.visit(visitor, builder, context.tmplDef(), args)

      case _ =>
        defMarker.drop()
    }
  }
}
