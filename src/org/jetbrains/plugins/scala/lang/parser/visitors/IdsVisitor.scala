package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder
import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
object IdsVisitor extends VisitorHelper {
  override def visit(builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Object]): Unit = ???
}
