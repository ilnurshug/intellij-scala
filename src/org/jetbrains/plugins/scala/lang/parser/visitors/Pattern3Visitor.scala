package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.ScalaLangParser.Pattern3Context
import org.jetbrains.plugins.scala.lang.parser.{ErrMsg, ScalaElementTypes, ScalaLangVisitorImpl}
object Pattern3Visitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, ctx: ParserRuleContext): Unit = {
    val context = ctx.asInstanceOf[Pattern3Context]
    val builder = visitor.getBuilder


    type Stack[X] = _root_.scala.collection.mutable.Stack[X]
    val markerStack = new Stack[PsiBuilder.Marker]
    val opStack = new Stack[String]
    //val infixMarker = builder.mark
    var backupMarker = builder.mark
    var count: Int = 0

    visitor.visitSimplePattern(context.simplePattern())

    var curSubCtx = context.subPattern3()

    while (curSubCtx.simplePattern() != null) {
      count = count + 1
      val s = builder.getTokenText

      var exit = false
      while (!exit) {
        if (opStack.isEmpty) {
          opStack push s
          val newMarker = backupMarker.precede
          markerStack push newMarker
          exit = true
        }
        else if (!compar(s, opStack.top,builder)) {
          opStack.pop()
          backupMarker.drop()
          backupMarker = markerStack.top.precede
          markerStack.pop().done(ScalaElementTypes.INFIX_PATTERN)
        }
        else {
          opStack push s
          val newMarker = backupMarker.precede
          markerStack push newMarker
          exit = true
        }
      }
      val idMarker = builder.mark
      builder.advanceLexer() //Ate id
      idMarker.done(ScalaElementTypes.REFERENCE)

      backupMarker.drop()
      backupMarker = builder.mark

      visitor.visitSimplePattern(curSubCtx.simplePattern())
      curSubCtx = curSubCtx.subPattern3()
    }

    backupMarker.drop()
    if (count>0) {
      while (markerStack.nonEmpty) {
        markerStack.pop().done(ScalaElementTypes.INFIX_PATTERN)
      }
      //infixMarker.done(ScalaElementTypes.INFIX_PATTERN)
    }
    else {
      while (markerStack.nonEmpty) {
        markerStack.pop().drop()
      }
      //infixMarker.drop
    }
  }

  import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils.priority

  //compares two operators a id2 b id1 c
  private def compar(id1: String, id2: String, builder: PsiBuilder): Boolean = {
    if (priority(id1) < priority(id2)) true        //  a * b + c  =((a * b) + c)
    else if (priority(id1) > priority(id2)) false  //  a + b * c = (a + (b * c))
    else if (associate(id1) == associate(id2))
      if (associate(id1) == -1) true
      else false
    else {
      builder error ErrMsg("wrong.type.associativity")
      false
    }
  }
  private def opeq(id1: String, id2: String): Boolean = priority(id1) == priority(id2)
  //Associations of operator
  private def associate(id: String): Int = {
    id.charAt(id.length-1) match {
      case ':' => -1   // right
      case _   => +1  // left
    }
  }
}
