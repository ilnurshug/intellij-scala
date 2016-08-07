package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder
import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl
object SimpleExpr1Visitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = ???
}
