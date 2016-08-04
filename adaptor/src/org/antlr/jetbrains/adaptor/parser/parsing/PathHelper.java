package org.antlr.jetbrains.adaptor.parser.parsing;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.jetbrains.plugins.scala.lang.ScalaLangParser;
import org.jetbrains.plugins.scala.lang.parser.ScalaElementTypes;
import org.antlr.jetbrains.adaptor.parser.parsing.Helper;

import java.util.ArrayDeque;
import java.util.Deque;

public class PathHelper extends ParserRuleNodeContextHelper implements Helper {
    @Override
    public void visitTerminal(TerminalNode node, PsiBuilder builder) {}
    @Override
    public void exitEveryRule(ParserRuleContext ctx, PsiBuilder.Marker marker, final Deque<PsiBuilder.Marker> markers) {
        int r = ctx.getParent().getRuleIndex();
        if (hasTerminalNode(ctx, ScalaLangParser.THIS)) {
            marker.done(ScalaElementTypes.THIS_REFERENCE());
        }
        else if (r == ScalaLangParser.RULE_simpleExpr) {
            marker.done(ScalaElementTypes.REFERENCE_EXPRESSION());
        }
        else if (r == ScalaLangParser.RULE_simpleType) {
            marker.done(ScalaElementTypes.REFERENCE());
        }
        else {
            marker.done(ScalaElementTypes.REFERENCE_EXPRESSION());
        }
    }
}
