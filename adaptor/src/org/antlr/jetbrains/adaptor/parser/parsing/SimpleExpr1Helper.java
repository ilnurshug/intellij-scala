package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class SimpleExpr1Helper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        int childCount = ctx.getChildCount();
        if (childCount > 1 && ctx.getChild(childCount - 1) instanceof RuleNode) {
            RuleNode lastChild = (RuleNode)ctx.getChild(childCount - 1);
            if (lastChild.getRuleContext().getRuleIndex() == ScalaLangParser.RULE_argumentExprs) {
                marker.done(ScalaElementTypes.METHOD_CALL());
            }
            else {
                marker.drop();
            }
        }
        else {
            marker.drop();
        }
    }
}
