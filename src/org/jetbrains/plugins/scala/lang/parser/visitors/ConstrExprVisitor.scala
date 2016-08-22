package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.ConstrExprContext
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
object ConstrExprVisitor extends VisitorHelper[ConstrExprContext] {
  override def visit(visitor: ScalaLangVisitorImpl, context: ConstrExprContext): Unit = {
    val builder = visitor.builder
    val marker = builder.mark()

    if (context.selfInvocation() != null) {
      visitor.visitSelfInvocation(context.selfInvocation())
      marker.done(ScalaElementTypes.CONSTR_EXPR)
    }
    else {
      visitor.visitConstrBlock(context.constrBlock())
      marker.drop()
    }
  }
}
