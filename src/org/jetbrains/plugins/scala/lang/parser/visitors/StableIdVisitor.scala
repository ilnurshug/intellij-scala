package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder
import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.parser.ScalaLangVisitorImpl
object StableIdVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val forImport = if (args.isEmpty) false else args.pop()
    val element = visitor.typeArgs.pop()
    // TODO: rewrite this (see details in stableId.scala)
    val marker = builder.mark()
    visitor.visitChildren(ctx)
    marker.done(element)
  }
}
