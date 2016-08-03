package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.jetbrains.adaptor.parser.ANTLRParseTreeToPSIConverter;
import org.antlr.jetbrains.adaptor.parser.CustomParseTreeWalker;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.*;

public class InfixTypeHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        //marker.drop();
    }

    public static class InfixTypeWalker implements CustomParseTreeWalker.WalkProc {
        @Override
        public void walk(ANTLRParseTreeToPSIConverter listener, ParseTree t) {
            PsiBuilder builder = listener.getBuilder();

            PsiBuilder.Marker infixTypeMarker = builder.mark();
            Stack<PsiBuilder.Marker> markerList = new Stack<PsiBuilder.Marker>(); //This list consist of markers for right-associated op
            int count = 0;
            markerList.push(infixTypeMarker);

            /*builder.getTokenType match {
                case ScalaTokenTypes.tUNDER => //wildcard is possible for infix types, like for parameterized. No bounds possible
                    val typeMarker = builder.mark()
                    builder.advanceLexer()
                    typeMarker.done(ScalaElementTypes.WILDCARD_TYPE)
                    builder.getTokenText match {
                    case "<:" | ">:" =>
                        infixTypeMarker.rollbackTo()
                        return false
                    case _ =>
                }
                case _ =>
                    if (!componentType.parse(builder, star, isPattern)) {
                        infixTypeMarker.rollbackTo()
                        return false
                    }
            }*/
            CustomParseTreeWalker.DEFAULT.walk(listener, t.getChild(0));
            int i = 1;

            int assoc = 0;  //this mark associativity: left - 1, right - -1
            while (i < t.getChildCount()) {
                count = count+1;
                //need to know associativity
                String s = builder.getTokenText();
                switch (s.charAt(s.length()-1)) {
                    case ':' :
                        switch(assoc) {
                            case 0:
                                assoc = -1;
                                break;
                            case 1:
                                builder.error("wrong.type.associativity");
                                break;
                            case -1:
                                break;
                        }
                        break;

                    default:
                        switch(assoc) {
                            case 0:
                                assoc = 1;
                                break;
                            case 1:
                                break;
                            case -1:
                                builder.error("wrong.type.associativity");
                                break;
                        }
                }
                PsiBuilder.Marker idMarker = builder.mark();
                //builder.advanceLexer() //Ate id
                CustomParseTreeWalker.DEFAULT.walk(listener, t.getChild(i));
                i++;
                idMarker.done(ScalaElementTypes.REFERENCE());

                if (assoc == -1) {
                    PsiBuilder.Marker newMarker = builder.mark();
                    markerList.push(newMarker);
                }
                /*if (builder.twoNewlinesBeforeCurrentToken) {
                    builder.error(errorMessage)
                }*/
                /*builder.getTokenType match {
                    case ScalaTokenTypes.tUNDER => //wildcard is possible for infix types, like for parameterized. No bounds possible
                        val typeMarker = builder.mark()
                        builder.advanceLexer()
                        typeMarker.done(ScalaElementTypes.WILDCARD_TYPE)
                    case _ =>
                        if (!componentType.parse(builder, star, isPattern)) builder error errorMessage
                }*/
                CustomParseTreeWalker.DEFAULT.walk(listener, t.getChild(i));
                i++;

                if (assoc == 1) {
                    PsiBuilder.Marker newMarker = infixTypeMarker.precede();
                    infixTypeMarker.done(ScalaElementTypes.INFIX_TYPE());
                    infixTypeMarker = newMarker;
                }
            }
            //final ops closing
            if (count>0) {
                if (assoc == 1) {
                    infixTypeMarker.drop();
                }
                else {
                    markerList.pop().drop();
                    while(!markerList.isEmpty()) {
                        PsiBuilder.Marker x = markerList.pop();
                        x.done(ScalaElementTypes.INFIX_TYPE());
                    }
                }
            }
            else {
                if (assoc == 1) {
                    infixTypeMarker.drop();
                }
                else {
                    while(!markerList.isEmpty()) markerList.pop().drop();
                }
            }

        }
    }
}
