package org.jetbrains.plugins.scala.lang.parser.visitors
import com.intellij.lang.PsiBuilder
import org.jetbrains.plugins.scala.ScalaBundle
import org.jetbrains.plugins.scala.lang.ScalaLangParser
import org.jetbrains.plugins.scala.lang.ScalaLangParser.TypeContext
import org.jetbrains.plugins.scala.lang.lexer.ScalaTokenTypes
import org.jetbrains.plugins.scala.lang.parser.{ScalaElementTypes, ScalaLangVisitorImpl}
import org.jetbrains.plugins.scala.lang.parser.parsing.types.ExistentialClause

import scala.collection.mutable
import org.antlr.v4.runtime.ParserRuleContext
import org.jetbrains.plugins.scala.lang.parser.visitors._

object TypeVisitor extends VisitorHelper {
  override def visit(visitor: ScalaLangVisitorImpl, builder: PsiBuilder, ctx: ParserRuleContext, args: mutable.Stack[Boolean]): Unit = {
    val isPattern: Boolean = args.pop()
    val star: Boolean = args.pop()
    var i = 0

    val context: TypeContext = ctx.asInstanceOf[TypeContext]

    val typeMarker = builder.mark
    if (context.infixType() == null) { // !infixType.parse(builder, star, isPattern)

      builder.getTokenType match {
        case ScalaTokenTypes.tUNDER =>
          builder.advanceLexer()
          builder.getTokenText match {
            case ">:" =>
              builder.advanceLexer()

              args.push(false); args.push(false)
              visit(visitor, builder, context.`type`(i), args)
              i = i + 1

              /*if (!parse(builder)) {
                builder error ScalaBundle.message("wrong.type")
              }*/
            case _ => //nothing
          }
          builder.getTokenText match {
            case "<:" =>
              builder.advanceLexer()

              args.push(false); args.push(false)
              visit(visitor, builder, context.`type`(i), args)
              i = i + 1

              /*if (!parse(builder)) {
                builder error ScalaBundle.message("wrong.type")
              }*/
            case _ => //nothing
          }
          typeMarker.done(ScalaElementTypes.WILDCARD_TYPE)
          builder.getTokenType match {
            case ScalaTokenTypes.tFUNTYPE =>
              val funMarker = typeMarker.precede()
              builder.advanceLexer() //Ate =>

              args.push(false); args.push(isPattern)
              visit(visitor, builder, context.`type`(i), args)
              i = i + 1

              /*if (!parse(builder, star = false, isPattern = isPattern)) {
                builder error ScalaBundle.message("wrong.type")
              }*/
              funMarker.done(ScalaElementTypes.TYPE)
            case _ =>
          }
          return
        case _ =>
          typeMarker.drop()
          return
      }
    }
    else {
      //infixType.parse(builder, star, isPattern)
      args.push(star)
      args.push(isPattern)
      InfixTypeVisitor.visit(visitor, builder, context.infixType(), args)
    }


    builder.getTokenType match {
      case ScalaTokenTypes.tFUNTYPE =>
        builder.advanceLexer() //Ate =>
        /*if (!parse(builder, star = false, isPattern = isPattern)) {
          builder error ScalaBundle.message("wrong.type")
        }*/
        args.push(false); args.push(isPattern)
        visit(visitor, builder, context.`type`(i), args)
        i = i + 1

        typeMarker.done(ScalaElementTypes.TYPE)
      case ScalaTokenTypes.kFOR_SOME =>
        //ExistentialClause parse builder

        ExistentialClauseVisitor.visit(visitor, builder, context.infixType(), args)

        typeMarker.done(ScalaElementTypes.EXISTENTIAL_TYPE)
      case _ => typeMarker.drop()
    }
    //true
  }
}
