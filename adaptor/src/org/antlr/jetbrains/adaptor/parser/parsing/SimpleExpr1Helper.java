package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.lexer.ScalaElementType;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class SimpleExpr1Helper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        int childCount = ctx.getChildCount();
        if (childCount > 1) {
            if (ctx.getChild(childCount - 1) instanceof RuleNode) {
                RuleNode lastChild = (RuleNode) ctx.getChild(childCount - 1);
                int r = lastChild.getRuleContext().getRuleIndex();
                if (r == ScalaLangParser.RULE_argumentExprs) {
                    marker.done(ScalaElementTypes.METHOD_CALL());
                } else if (r == ScalaLangParser.RULE_typeArgs) {
                    marker.done(ScalaElementTypes.GENERIC_CALL());
                } else if (r == ScalaLangParser.RULE_id && hasTerminalNode(ctx, ScalaLangParser.DOT)) {
                    marker.done(ScalaElementTypes.REFERENCE_EXPRESSION());
                } else {
                    marker.drop();
                }
            }
            else {
                TerminalNode lastChild = (TerminalNode)ctx.getChild(childCount - 1);
                int type = lastChild.getSymbol().getType();
                if (type == ScalaLangParser.RPARENTHESIS) {
                    if (hasTerminalNode(ctx, ScalaLangParser.RULE_exprs)) {
                        if (hasTerminalNode(ctx, ScalaLangParser.COMMA)) {
                            marker.done(ScalaElementTypes.TUPLE());
                        }
                        else {
                            marker.done(ScalaElementTypes.PARENT_EXPR());
                        }
                    }
                    else {
                        marker.done(ScalaElementTypes.UNIT_EXPR());
                    }
                }
            }
        }
        else {
            marker.drop();
        }
    }
}
