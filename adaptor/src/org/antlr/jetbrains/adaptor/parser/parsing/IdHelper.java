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

public class IdHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        RuleNode p = (RuleNode)ctx.getParent();
        int r = p.getRuleContext().getRuleIndex();
        if (r == ScalaLangParser.RULE_infixExpr) {
            marker.done(ScalaElementTypes.REFERENCE_EXPRESSION());
        }
        /*else if (r == ScalaLangParser.RULE_pattern3) {
            marker.done(ScalaElementTypes.REFERENCE());
        }*/
        else if (r == ScalaLangParser.RULE_ids) {
            marker.done(ScalaElementTypes.FIELD_ID());
        }
        else {
            marker.drop();
        }
    }
}
