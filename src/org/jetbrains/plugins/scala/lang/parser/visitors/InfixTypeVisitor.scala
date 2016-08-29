package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.lang.ScalaLangParser.InfixTypeContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}

/*
infixType         : compoundOrWildType (id  Nl? compoundOrWildType )*;
 */

object InfixTypeVisitor extends VisitorHelper[InfixTypeContext] {

  override def visit(visitor: ScalaLangVisitorImpl, context: InfixTypeContext): Unit = {
    val builder: PsiBuilder = visitor.builder

    var typeIdx = 0
    val typeCount = context.compoundOrWildType().size()

    var infixTypeMarker = builder.mark
    var markerList = List[PsiBuilder.Marker]() //This list consist of markers for right-associated op
    var count = 0
    markerList = infixTypeMarker :: markerList

    visitor.visitCompoundOrWildType(context.compoundOrWildType(typeIdx))
    typeIdx += 1

    var assoc: Int = 0  //this mark associativity: left - 1, right - -1
    while (typeIdx < typeCount) {
      count = count+1
      //need to know associativity
      val s = builder.getTokenText
      s.charAt(s.length-1) match {
        case ':' =>
          assoc match {
            case 0  => assoc = -1
            case 1  => builder error ScalaBundle.message("wrong.type.associativity")
            case -1 =>
          }
        case _ =>
          assoc match {
            case 0  => assoc = 1
            case 1  =>
            case -1 => builder error ScalaBundle.message("wrong.type.associativity")
          }
      }
      val idMarker = builder.mark
      builder.advanceLexer() //Ate id
      idMarker.done(ScalaElementTypes.REFERENCE)

      if (assoc == -1) {
        val newMarker = builder.mark
        markerList = newMarker :: markerList
      }

      visitor.visitCompoundOrWildType(context.compoundOrWildType(typeIdx))
      typeIdx += 1

      if (assoc == 1) {
        val newMarker = infixTypeMarker.precede
        infixTypeMarker.done(ScalaElementTypes.INFIX_TYPE)
        infixTypeMarker = newMarker
      }
    }

    //final ops closing
    if (count>0) {
      if (assoc == 1) {
        infixTypeMarker.drop()
      }
      else {
        markerList.head.drop()
        for (x: PsiBuilder.Marker <- markerList.tail) x.done(ScalaElementTypes.INFIX_TYPE)
      }
    }
    else {
      if (assoc == 1) {
        infixTypeMarker.drop()
      }
      else {
        for (x: PsiBuilder.Marker <- markerList) x.drop()
      }
    }

  }
}
