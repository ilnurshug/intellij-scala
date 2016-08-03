package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.jetbrains.adaptor.parser.ANTLRParseTreeToPSIConverter;
import org.antlr.jetbrains.adaptor.parser.CustomParseTreeWalker;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class ResultExprHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        //marker.drop();
    }

    public static class ResultExprWalker implements CustomParseTreeWalker.WalkProc {
        private int i = 0;

        int walkNextSubTree(ANTLRParseTreeToPSIConverter listener, ParseTree t) {
            CustomParseTreeWalker.DEFAULT.walk(listener, t.getChild(i));
            i++;

            return i;
        }

        int currentSubtreeRule(ParseTree t) {
            ParseTree c = t.getChild(i);
            if (c instanceof RuleNode) return ((RuleNode) c).getRuleContext().getRuleIndex();
            else return -1;
        }

        int currentSubtreeTokenType(ParseTree t) {
            ParseTree c = t.getChild(i);
            if (c instanceof TerminalNode) return ((TerminalNode) c).getSymbol().getType();
            else return -1;
        }

        @Override
        public void walk(ANTLRParseTreeToPSIConverter listener, ParseTree t) {
            i = 0;
            PsiBuilder builder = listener.getBuilder();
            PsiBuilder.Marker resultMarker = builder.mark();
            PsiBuilder.Marker pmarker = null;

            if (t.getChild(i) instanceof TerminalNode) {
                switch (currentSubtreeTokenType(t)) {
                    case ScalaLangParser.IMPLICIT:
                        pmarker = builder.mark();
                        //builder.advanceLexer() //ate implicit
                        walkNextSubTree(listener, t);

                        switch (currentSubtreeRule(t)) {
                            case ScalaLangParser.RULE_id:
                                parseFunction(listener, t, pmarker, resultMarker);
                                return;
                            default:
                                resultMarker.drop();
                                return;
                        }

                    case ScalaLangParser.UNDER:
                        pmarker = builder.mark();
                        parseFunction(listener, t, pmarker, resultMarker);
                        return;
                    default:
                        break;
                }
            }
            else {
                switch (currentSubtreeRule(t)) {
                    case ScalaLangParser.RULE_id:
                        pmarker = builder.mark();
                        parseFunction(listener, t, pmarker, resultMarker);
                        break;
                    case ScalaLangParser.RULE_bindings:
                        //bindings parse builder
                        walkNextSubTree(listener, t);

                        parseFunctionEnd(listener, t, resultMarker);
                        break;
                    default:
                        walkNextSubTree(listener, t);
                        resultMarker.drop();
                        break;
                }
            }
        }

        private void parseFunctionEnd(ANTLRParseTreeToPSIConverter listener, ParseTree t, PsiBuilder.Marker resultMarker) {
            PsiBuilder builder = listener.getBuilder();
            switch(currentSubtreeTokenType(t)) {
                case ScalaLangParser.FUNTYPE :
                    //builder.advanceLexer() //Ate =>
                    walkNextSubTree(listener, t);

                    walkNextSubTree(listener, t);

                    resultMarker.done(ScalaElementTypes.FUNCTION_EXPR());
                    break;
                default:
                    resultMarker.drop();
                    break;
            }
        }

        private void parseFunction(ANTLRParseTreeToPSIConverter listener, ParseTree t, PsiBuilder.Marker paramsMarker, PsiBuilder.Marker resultMarker) {
            PsiBuilder builder = listener.getBuilder();

            PsiBuilder.Marker paramMarker = builder.mark();

            //builder.advanceLexer() //Ate id
            walkNextSubTree(listener, t);

            System.out.println(currentSubtreeTokenType(t) + " " + builder.getTokenText());

            if (ScalaLangParser.COLON == currentSubtreeTokenType(t)) {
                // builder.advanceLexer() // ate ':'
                walkNextSubTree(listener, t);

                PsiBuilder.Marker pt = builder.mark();

                //`type`.parse(builder, isPattern = false)
                walkNextSubTree(listener, t);

                pt.done(ScalaElementTypes.PARAM_TYPE());
            }
            switch(currentSubtreeTokenType(t)) {
                case ScalaLangParser.FUNTYPE :
                    PsiBuilder.Marker psm = paramsMarker.precede(); // 'parameter list'
                    paramMarker.done(ScalaElementTypes.PARAM());
                    paramsMarker.done(ScalaElementTypes.PARAM_CLAUSE());
                    psm.done(ScalaElementTypes.PARAM_CLAUSES());

                    parseFunctionEnd(listener, t, resultMarker);
                    return;
                default:
                    builder.error("fun.sign.expected");
            }
            parseFunctionEnd(listener, t, resultMarker);
        }
    }
}
