package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.RuleNode;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class TypeHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        int childCount = ctx.getChildCount();

        if (childCount > 1) {
            ParseTree firstChild = ctx.getChild(0);
            if (firstChild instanceof RuleNode) {
                int rule = ((RuleNode) firstChild).getRuleContext().getRuleIndex();
                if (rule == ScalaLangParser.RULE_infixType) {
                    if (hasTerminalNode(ctx, "=>"))
                        marker.done(ScalaElementTypes.TYPE());
                    else if (hasTerminalNode(ctx, "forSome"))
                        marker.done(ScalaElementTypes.EXISTENTIAL_TYPE());
                }
            }
        }
        else {
            marker.drop();
        }
    }

    private boolean hasTerminalNode(ParserRuleContext ctx, String terminal) {
        boolean f = false;
        int childCount = ctx.getChildCount();
        for (int i = 0; i < childCount; i++) {
            if (ctx.getChild(i).getText().compareTo(terminal) == 0) f = true;
        }

        return f;
    }
}