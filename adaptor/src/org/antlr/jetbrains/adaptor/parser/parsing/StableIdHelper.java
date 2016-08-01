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

public class StableIdHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        RuleNode parent = (RuleNode)ctx.getParent();
        int r = parent.getRuleContext().getRuleIndex();
        if (r == ScalaLangParser.RULE_simpleType) {
            marker.done(ScalaElementTypes.REFERENCE());
        }
        else if (r == ScalaLangParser.RULE_stableId) {
            marker.done(ScalaElementTypes.REFERENCE());
        }
        else if (r == ScalaLangParser.RULE_importExpr) {
            marker.done(ScalaElementTypes.REFERENCE());
        }
        else {
            marker.drop();
        }
    }
}
