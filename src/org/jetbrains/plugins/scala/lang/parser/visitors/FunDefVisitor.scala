package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.FunDefContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl

/*
funDef            : funSig ( ':'  type)?  '='  expr
                  | funSig  Nl?  '{'  block  '}'
                  | 'this'  paramClause  paramClauses ('='  constrExpr |  Nl  constrBlock) ;

 */

object FunDefVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val context: FunDefContext = ctx.asInstanceOf[FunDefContext]

    if (context.block() != null) {
      FunSigVisitor.visit(visitor, builder, context.funSig(), args)

      args.push(true); args.push(false)
      BlockVisitor.visit(visitor, builder, context.block(), args)
    }
    else {
      visitor.visitChildren(context)
    }
  }
}
