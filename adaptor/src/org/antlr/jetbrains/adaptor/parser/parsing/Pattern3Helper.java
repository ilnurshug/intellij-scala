package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.jetbrains.adaptor.parser.ANTLRParseTreeToPSIConverter;
import org.antlr.jetbrains.adaptor.parser.CustomParseTreeWalker;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Stack;

import org.jetbrains.plugins.scala.lang.parser.util.ParserUtils;

public class Pattern3Helper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        // unnecessary code since we using Pattern3Walker
        // marker.drop();
        return;
    }

    public static class Pattern3Walker implements CustomParseTreeWalker.WalkProc {
        @Override
        public void walk(ANTLRParseTreeToPSIConverter listener, ParseTree t) {
            if(t instanceof ErrorNode) {
                listener.visitErrorNode((ErrorNode)t);
                return;
            }

            Stack<PsiBuilder.Marker> markerStack = new Stack<PsiBuilder.Marker>();
            Stack<String> opStack = new Stack<String>();
            PsiBuilder builder = listener.getBuilder();
            PsiBuilder.Marker backupMarker = builder.mark();
            int count = 0;

            // simplePattern.parse(builder)
            CustomParseTreeWalker.DEFAULT.walk(listener, t.getChild(0));
            int i = 1;

            while (i < t.getChildCount()) {
                count = count + 1;
                String s = builder.getTokenText();

                boolean exit = false;
                while (!exit) {
                    if (opStack.isEmpty()) {
                        opStack.push(s);
                        PsiBuilder.Marker newMarker = backupMarker.precede();
                        markerStack.push(newMarker);
                        exit = true;
                    }
                    else if (!compar(s, opStack.peek(), builder)) {
                        opStack.pop();
                        backupMarker.drop();
                        backupMarker = markerStack.peek().precede();
                        markerStack.pop().done(ScalaElementTypes.INFIX_PATTERN());
                    }
                    else {
                        opStack.push(s);
                        PsiBuilder.Marker newMarker = backupMarker.precede();
                        markerStack.push(newMarker);
                        exit = true;
                    }
                }
                PsiBuilder.Marker idMarker = builder.mark();

                //builder.advanceLexer(); //Ate id
                CustomParseTreeWalker.DEFAULT.walk(listener, t.getChild(i));
                i++;

                idMarker.done(ScalaElementTypes.REFERENCE());


                backupMarker.drop();
                backupMarker = builder.mark();

                //simplePattern.parse(builder);
                CustomParseTreeWalker.DEFAULT.walk(listener, t.getChild(i));
                i++;
            }
            backupMarker.drop();
            if (count>0) {
                while (!markerStack.isEmpty()) {
                    markerStack.pop().done(ScalaElementTypes.INFIX_PATTERN());
                }
            }
            else {
                while (!markerStack.isEmpty()) {
                    markerStack.pop().drop();
                }
            }
        }

        private boolean compar(String id1, String id2, PsiBuilder builder)  {
            if (ParserUtils.priority(id1,false) < ParserUtils.priority(id2,false))
                return true;        //  a * b + c  =((a * b) + c)
            else if (ParserUtils.priority(id1,false) > ParserUtils.priority(id2,false))
                return false;       //  a + b * c = (a + (b * c))
            else if (associate(id1) == associate(id2))
                return (associate(id1) == -1);
            else {
                builder.error("wrong.type.associativity");
                return false;
            }
        }

        private int associate(String id) {
            switch (id.charAt(id.length()-1))  {
                case ':': return -1;   // right
                default:  return +1;   // left
            }
        }
    }
}
